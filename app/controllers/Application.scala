package controllers

import scala.concurrent.ExecutionContext.Implicits.global
import org.joda.time.DateTime
import edu.knowitall.common.Resource.using
import edu.knowitall.common.Timing
import models.FreeBaseType
import models.AnswerSet
import models.LogEntry
import models.PositiveTypeFilter
import models.TripleQuery
import models.QAQuery
import models.Query
import models.TypeFilter
import models.TypeFilters
import play.api.Logger
import play.api.Play.current
import play.api.cache.Cache
import play.api.data.Form
import play.api.data.Forms.tuple
import play.api.data.Forms.mapping
import play.api.data.Forms.optional
import play.api.data.Forms.text
import play.api.mvc.Action
import play.api.mvc.Controller
import play.api.mvc.RequestHeader
import controllers.Executor.ExecutionSettings
import play.api.mvc.Request
import play.api.mvc.AnyContent

object Application extends Controller {
  final val PAGE_SIZE = 20
  final val MAX_SENTENCE_COUNT = 10
  final val MAX_GROUP_COUNT = 5
  final val DEFAULT_PARAPHRASER = "templatesLm"
  final val DEFAULT_PARSER = "regex"

  Logger.info("Server started.")

  /**
    * The actual definition of the search form.
    */
  def searchForm: Form[Query] = {

    Form(
    // Defines a mapping that will handle Contact values
      (mapping (
        "question" -> text
      )(t => Query.apply(t, DEFAULT_PARAPHRASER, DEFAULT_PARSER))(q => Query.unapply(q).map(_._1)))
    )
  }

  def footer(reload: Boolean = false): String = {
    def loadFooter =
      try {
        val footerFile = new java.io.File("/cse/www2/knowitall/footer.html")
        val footer =
          using (scala.io.Source.fromFile(footerFile)) { file =>
            file.mkString
          }
        Cache.set("footer", footer)
        footer
      } catch {
        case e: Exception => Logger.error("Exception loading footer." + e); ""
      }

    if (reload) {
      loadFooter
    }
    else {
      Cache.getAs[String]("footer").getOrElse(loadFooter)
    }
  }

  /**
    * This is the index page that hold the search form.
    */
  def index(reloadFooter: Boolean) = Action {
    Ok(views.html.index(searchForm, footer(reloadFooter)))
  }

  private def settingsFromRequest(debug: Boolean, request: Request[AnyContent]) = {
    var settings = Executor.ExecutionSettings.default
    if (debug) {
      val entityThresh: Option[Double] = request.queryString.get("entityThresh").flatMap(_.headOption.map(_.toDouble))

      entityThresh.foreach(thresh => settings = settings.copy(entityScoreThreshold = thresh))
    }
    settings
  }

  /**
    * Handle POST requests to search.
    */
  def submit(debug: Boolean = false) = Action { implicit request =>

    searchForm.bindFromRequest.fold(
      errors => { println(errors); BadRequest(views.html.index(errors, footer())) },
      query => submitHelper(query, debug)
    )
  }

  private def submitHelper(query: Query, debug: Boolean)(implicit request: Request[AnyContent]) = {

    val answers = scala.concurrent.future {
      searchGroups(query, settingsFromRequest(debug, request), None, debug)
    }

    Async {
      answers.map {
        case (answers, message) => {
//          val filtered = setupFilters(answers, "all", 0, None)._2

          LogEntry.fromRequest(query, "all", answers.answerCount, answers.resultsCount, request).log()

          //choose a cut-off to filter out the entities that have few
          //results, and only display to a max of 7 entities
//          val ambiguousEntities = filtered.queryEntities.zipWithIndex.filter {
//            case ((fbe, entityCount), index) => index < 7 && entityCount > 5
//          }

          doSearch(query, "all", 0, None, settingsFromRequest(debug, request), debug = debug)

//          if (ambiguousEntities.size == 0) {
//            //when there is no entity that satisfy the cut-off filter above
//            //i.e, when results number is too small, do the regular query search.
//            doSearch(query, "all", 0, settingsFromRequest(debug, request), debug = debug)
//
//            // not sure what to do about disambiguation yet...
////          } else if (ambiguousEntities.size == 1) {
////            //when there is only a single entity present after the filter
////            //go directly to the linked entity query search
////            query.arg2.map(_.toString) match {
////              case Some(x) => doSearch(TripleQuery.fromStrings(query.arg1.map(_.toString), query.rel.map(_.toString), Option("entity:" + ambiguousEntities(0)._1._1.name), query.corpora.map(_.toString)), "all", 0, settingsFromRequest(debug, request), debug = debug)
////              case None    => doSearch(TripleQuery.fromStrings(Option("entity:" + ambiguousEntities(0)._1._1.name), query.rel.map(_.toString), query.arg2.map(_.toString), query.corpora.map(_.toString)), "all", 0, settingsFromRequest(debug, request), debug = debug)
////            }
//          } else {
//            //if there are more than 1 entities that are ambiguous
//            //direct to the disambiguation page and display an query-card for each
//            disambiguate(query, settingsFromRequest(debug, request), debug = debug)
//          }
        }
      }
    }
  }

  /**
   * Do the filtering of answers according to the query, answerSet and filterString.
   *
   * @return a tuple of (filters, filtered results, and single page of filtered results)
   */
  private def setupFilters(answers: AnswerSet, filterString: String, pageNumber : Int) = {
      val filters: Set[TypeFilter] = filterString match {
        case "" | "all" => Set()
        case "misc" => answers.filters.map(_.filter).collect { case filter: PositiveTypeFilter => filter.negate }
        case s =>
          val typ = FreeBaseType.parse(s).getOrElse(throw new IllegalArgumentException("invalid type string: " + s))
          Set(PositiveTypeFilter(typ, answers.attrs))
      }

      val filtered = answers filter filters
      Logger.info("Query with " + filters + " has " + filtered.answerCount + " answers " + filtered.resultsCount + " results")
      val page = filtered.page(pageNumber, PAGE_SIZE)

      (filters, filtered, page)
  }

  def search(question: String, parser: String, filter: String, page: Int, debug: Boolean, log: Boolean) = Action { implicit request =>
    doSearch(new Query(question, DEFAULT_PARAPHRASER, parser), filter, page, None, settingsFromRequest(debug, request), debug=debug, log=log)
  }

  def results(question: String, parser: String, filterString: String, pageNumber: Int, ppIndex: Option[Int], justResults: Boolean, debug: Boolean = false) = Action { implicit request =>
    doSearch(new Query(question, DEFAULT_PARAPHRASER, parser), filterString, pageNumber, ppIndex, settingsFromRequest(debug, request), debug=debug, log=true, justResults=justResults)
  }

  
  def sentences(question: String, parser: String, title: String, maxSentenceCount: Int, ppIndex: Option[Int], debug: Boolean) = Action {
    val query = new Query(question, DEFAULT_PARAPHRASER, parser)
    Logger.info("Sentences request for title '" + title + "' in: " + query)
    val group = searchGroups(query, ExecutionSettings.default, ppIndex, debug)._1.answers.find(_.title == title) match {
      case None => throw new IllegalArgumentException("could not find group title: " + title)
      case Some(group) => group
    }

    Ok(views.html.sentences(group, maxSentenceCount, debug, index=0))
  }

  def logsFromDate(date: DateTime = DateTime.now) =
    logs(date.getYear, date.getMonthOfYear, date.getDayOfMonth)

  def logs(year: Int, month: Int, day: Int) = Action {
    val today = new DateTime(year, month, day, 0, 0, 0, 0)

    Ok(views.html.logs(LogEntry.logs(year, month, day), today))
  }

  def searchGroups(query: Query, settings: ExecutionSettings, ppIndex: Option[Int], debug: Boolean) = {
    Logger.debug("incoming " + query)
    Cache.getAs[AnswerSet](query.toString.toLowerCase) match {
      case Some(answers) if !debug =>
        Logger.debug("retrieving " + query + " from cache")

        val AnswerSet(groups, filters, entities, paraphraseHits) = answers

        // cache hit
        Logger.info(query.toString +
          " retrieved from cache" +
          " with " + groups.size + " answers" +
          " and " + groups.iterator.map(_.resultsCount).sum + " results")
          
        val ppLimited = ppIndex.map(answers.exactPP).getOrElse(answers)
        (ppLimited, Some("cached"))
      case _ =>
        Logger.debug("executing " + query + " in lucene")

        // cache miss
        val (ns, result) = Timing.time(Executor.execute(query, settings))

        val (groups, message) = result match {
          case Executor.Success(groups) => (groups, None)
          case Executor.Timeout(groups) => (groups, Some("timeout"))
          case Executor.Limited(groups) => (groups, Some("results truncated"))
        }

        val answers = AnswerSet.from(groups, TypeFilters.fromGroups(groups, debug))

        Logger.info(query.toString +
          " executed in " + Timing.Seconds.format(ns) +
          " with " + groups.size + " answers" +
          " and " + groups.iterator.map(_.resultsCount).sum + " results" + message.map(" (" + _ + ")").getOrElse(""))

        // cache unless we had a timeout
        if (!result.isInstanceOf[Executor.Timeout[_]]) {
          Logger.debug("Saving " + query.toString + " to cache.")
          Cache.set(query.toString.toLowerCase, answers, 60 * 10)
        }
        val ppLimited = ppIndex.map(answers.exactPP).getOrElse(answers)
        (ppLimited, message)
    }
  }

  def doSearch(query: Query, filterString: String, pageNumber: Int, ppIndex: Option[Int], settings: ExecutionSettings, debug: Boolean = false, log: Boolean = true, justResults: Boolean = false)(implicit request: RequestHeader) = {
    Logger.info("Search request: " + query)

    val maxQueryTime = 20 * 1000 /* ms */

    val answers = scala.concurrent.future {
      searchGroups(query, settings, ppIndex, debug)
    }

    Async {
      answers.map { case (answers, message) =>
        val (filters, filtered, page) = setupFilters(answers, filterString, pageNumber)

        if (log) {
          LogEntry.fromRequest(query, filterString, answers.answerCount, answers.resultsCount, request).log()
        }

        //if only the category of results is clicked, change the page's result content
        //else generate a header with the result content
        if (justResults) {
          Ok(views.html.results(query, page, filters, filterString, pageNumber, ppIndex, math.ceil(filtered.answerCount.toDouble / PAGE_SIZE.toDouble).toInt, MAX_GROUP_COUNT, MAX_SENTENCE_COUNT, debug))
        } else {
          Ok(
            views.html.frame.resultsframe(
             searchForm, query, message, page, filtered.answerCount, filtered.resultsCount, true, ppIndex.isDefined)(
               views.html.results(query, page, filters, filterString, pageNumber, ppIndex, math.ceil(filtered.answerCount.toDouble / PAGE_SIZE.toDouble).toInt, MAX_GROUP_COUNT, MAX_SENTENCE_COUNT, debug)))
        }
      }
    }
  }

//  def disambiguate(query: Query, settings: ExecutionSettings, debug: Boolean = false, log: Boolean = true)(implicit request: RequestHeader) = {
//    val maxQueryTime = 20 * 1000 /* ms */
//
//    val answers = scala.concurrent.future {
//      searchGroups(query, settings, debug)
//    }
//
//    Async {
//      val filterString = "all"
//
//      answers.map { case (answers, message) =>
//        val filter = setupFilters(answers, filterString, 0)
//
//        if (log) {
//          LogEntry.fromRequest(query, filterString, answers.answerCount, answers.sentenceCount, request).log()
//        }
//
//        //choose a cut-off to filter out the entities that have few
//        //results, and only display to a max of 7 entities
//        val ambiguousEntitiesWithEntityCount = filter._2.queryEntities.zipWithIndex.filter{
//          case ((fbe, entityCount), index)  => index < 7 && entityCount > 5
//        }
//
//        //get the ambiguous Entities with their index and answerCount
//        val answer = filter._2.answers.flatMap(x => x.queryEntity)
//        val ambiguousEntitiesWithAnswerCount = for(((fbe, entityCount), index) <- ambiguousEntitiesWithEntityCount) yield {
//          val answerCount = answer.count(_._1.fbid == fbe.fbid)
//          (fbe, answerCount)
//        }
//
//        //sort the ambiguous entities according to the answer count in decreasing order.
//        val sortedAmbiguousEntitiesWithAnswerCount = ambiguousEntitiesWithAnswerCount.sortBy(-_._2)
//
//        //direct to disambiguate page with a resultsFrame header, and disambiguate
//        //query card contents.
//        Ok(
//          views.html.frame.resultsframe(
//            searchForm, query, message, filter._2, filter._2.answerCount, filter._2.sentenceCount, false)(
//              views.html.disambiguate(query, sortedAmbiguousEntitiesWithAnswerCount, filter._1.toSet, filterString, 0, math.ceil(filter._2.answerCount.toDouble / PAGE_SIZE.toDouble).toInt, MAX_SENTENCE_COUNT, debug)))
//      }
//    }
//  }
}
