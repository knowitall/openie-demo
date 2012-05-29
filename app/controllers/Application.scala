package controllers

import edu.washington.cs.knowitall.browser.extraction.FreeBaseType
import edu.washington.cs.knowitall.common.Timing

import models.{ TypeFilters, Query, PositiveTypeFilter, NegativeTypeFilter, LogEntry, AnswerSet }
import play.api.Play.current
import play.api.cache.Cache
import play.api.data.Forms.{ text, optional, mapping }
import play.api.data.Form
import play.api.mvc.{ Controller, Action }
import play.api.Logger
import play.api.libs.concurrent

object Application extends Controller {
  final val PAGE_SIZE = 30
  final val MAX_SENTENCE_COUNT = 30

  /**
    * The actual definition of the search form.
    */
  def searchForm: Form[Query] = {
    def unapply(query: Query): Option[(Option[String], Option[String], Option[String])] = {
      Some(query.arg1.map(_.toString), query.rel.map(_.toString), query.arg2.map(_.toString))
    }
    Form(
    // Defines a mapping that will handle Contact values
      (mapping (
        "arg1" -> optional(text),
        "rel" -> optional(text),
        "arg2" -> optional(text)
      )(Query.fromStrings)(unapply)).verifying("All search fields cannot be empty", { query =>
        query.arg1.isDefined || query.rel.isDefined || query.arg2.isDefined
      })
    )
  }

  def index = form

  /**
    * This is the index page that hold the search form.
    */
  def form = Action {
    Ok(views.html.index(searchForm))
  }

  /**
    * Handle POST requests to search.
    */
  def submit = Action { implicit request =>
    searchForm.bindFromRequest.fold(
      errors => BadRequest(views.html.index(errors)),
      query => doSearch(query, "all", 0))
  }

  def search(arg1: Option[String], rel: Option[String], arg2: Option[String], filter: String, page: Int, debug: Boolean) = Action {
    doSearch(Query.fromStrings(arg1, rel, arg2), filter, page, debug)
  }

  def sentences(arg1: Option[String], rel: Option[String], arg2: Option[String], title: String, debug: Boolean) = Action {
    val query = Query.fromStrings(arg1, rel, arg2)
    Logger.info("Showing sentences for title " + title + " in " + query)
    val group = searchGroups(query)._1.groups.find(_.title.text == title) match {
      case None => throw new IllegalArgumentException("could not find group title: " + title)
      case Some(group) => group
    }

    Ok(views.html.sentences(group, debug))
  }

  def logs(year: Int, month: Int, day: Int) = Action {
    Ok(views.html.logs(LogEntry.logs(year, month, day)))
  }

  def searchGroups(query: Query) = {
    Logger.debug("incoming " + query)
    Cache.getAs[AnswerSet](query.toString) match {
      case Some(answers) =>
        val AnswerSet(groups, filters) = answers

        // cache hit
        Logger.info(query.toString +
          " retrieved from cache" +
          " with " + groups.size + " answers" +
          " and " + groups.iterator.map(_.contents.size).sum + " results")
        (answers, Some("cache"))
      case None =>
        // cache miss
        val (ns, result) = Timing.time(query.execute())

        val (groups, message) = result match {
          case Query.Success(groups) => (groups, None)
          case Query.Timeout(groups, count) => (groups, Some("timeout"))
          case Query.Limited(groups, count) => (groups, Some("limited"))
        }

        val answers = AnswerSet.from(groups, TypeFilters.fromGroups(groups))

        Logger.info(query.toString +
          " executed in " + Timing.Seconds.format(ns) +
          " with " + groups.size + " answers" +
          " and " + groups.iterator.map(_.contents.size).sum + " sentences" + message.map(" (" + _ + ")").getOrElse(""))

        // cache unless we had a timeout
        if (!result.isInstanceOf[Query.Timeout]) {
          Logger.debug("Saving " + query.toString + " to cache.")
          Cache.set(query.toString, answers)
        }

        (answers, message)
    }
  }

  def results(arg1: Option[String], rel: Option[String], arg2: Option[String], filterString: String, pageNumber: Int, debug: Boolean = false) = Action {
    doSearch(Query.fromStrings(arg1, rel, arg2), filterString, pageNumber, debug, true)
  }

  def doSearch(query: Query, filterString: String, pageNumber: Int, debug: Boolean = false, justResults: Boolean = false) = {
    val maxQueryTime = 20 * 1000 /* ms */

    val answers = concurrent.Akka.future {
      searchGroups(query)
    }

    Async {
      answers.orTimeout("Query timeout after " + maxQueryTime + " ms due to high server load.", maxQueryTime).map {
        case Right(timeout) => Logger.warn(query.toString + "timed out after " + maxQueryTime + " ms"); InternalServerError(timeout)
        case Left((answers, message)) =>
          val filters = filterString match {
            case "" | "all" => Set()
            case "other" => answers.filters.map(filter => NegativeTypeFilter(filter._1.typ)).toSet
            case s => Set(PositiveTypeFilter(FreeBaseType.parse(s).getOrElse(throw new IllegalArgumentException("invalid type string: " + s))))
          }

          val filtered = answers filter filters
          Logger.info(query + " with " + filters + " has " + filtered.answerCount + " answers " + filtered.sentenceCount + " results")
          val page = filtered.page(pageNumber, PAGE_SIZE)

          val entry = new LogEntry(query, answers.answerCount, answers.sentenceCount)
          entry.log()

          if (justResults) {
            Ok(views.html.results(query, page, filters.toSet, filterString, pageNumber, math.ceil(filtered.answerCount.toDouble / PAGE_SIZE.toDouble).toInt, MAX_SENTENCE_COUNT, debug))
          } else {
            Ok(
              views.html.frame.resultsframe(
                searchForm, query, message, page, filtered.answerCount, filtered.sentenceCount)(
                  views.html.results(query, page, filters.toSet, filterString, pageNumber, math.ceil(filtered.answerCount.toDouble / PAGE_SIZE.toDouble).toInt, MAX_SENTENCE_COUNT, debug)))
          }
      }
    }
  }
}
