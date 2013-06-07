package controllers

import scala.annotation.implicitNotFound
import scala.concurrent.ExecutionContext.Implicits.global
import org.apache.commons.codec.binary.Base64
import org.joda.time.DateTime
import edu.knowitall.common.Resource.using
import edu.knowitall.common.Timing
import edu.knowitall.openie.models.Extraction
import edu.knowitall.openie.models.ExtractionGroupProtocol.listFormat
import edu.knowitall.openie.models.FreeBaseType
import edu.knowitall.openie.models.Instance
import edu.knowitall.openie.models.InstanceProtocol.InstanceFormat
import edu.knowitall.openie.models.serialize.Chill
import models.AnswerSet
import models.LogEntry
import models.NegativeTypeFilter
import models.PositiveTypeFilter
import models.Query
import models.TypeFilter
import models.TypeFilters
import play.api.Logger
import play.api.Play.current
import play.api.cache.Cache
import play.api.data.Form
import play.api.data.Forms.mapping
import play.api.data.Forms.optional
import play.api.data.Forms.text
import play.api.mvc.Action
import play.api.mvc.Controller
import play.api.mvc.RequestHeader
import sjson.json.JsonSerialization.tojson
import edu.knowitall.openie.models.ExtractionGroupProtocol
import edu.knowitall.openie.models.InstanceProtocol
import play.api.templates.Html

object Application extends Controller {
  final val PAGE_SIZE = 20
  final val MAX_SENTENCE_COUNT = 15

  /**
    * The actual definition of the search form.
    */
  def searchForm: Form[Query] = {
    def unapply(query: Query): Option[(Option[String], Option[String], Option[String], Option[String])] = {
      Some(query.arg1.map(_.toString), query.rel.map(_.toString), query.arg2.map(_.toString), query.corpora.map(_.toString))
    }
    Form(
    // Defines a mapping that will handle Contact values
      (mapping (
        "arg1" -> optional(text),
        "rel" -> optional(text),
        "arg2" -> optional(text),
        "corpora" -> optional(text)
      )(Query.fromStrings)(unapply)).verifying("All search fields cannot be empty", { query =>
        query.arg1.isDefined || query.rel.isDefined || query.arg2.isDefined
      })
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

  /**
    * Handle POST requests to search.
    */
  def submit(debug: Boolean = false) = Action { implicit request =>
    // val debug = request.queryString.get("debug").flatMap(_.headOption.map(_ == "1")).getOrElse(false)

    searchForm.bindFromRequest.fold(
      errors => BadRequest(views.html.index(errors, footer())),
      query => doSearch(query, "all", 0, debug=debug))
  }

  def search(arg1: Option[String], rel: Option[String], arg2: Option[String], filter: String, page: Int, debug: Boolean, log: Boolean, corpora: Option[String]) = Action { implicit request =>
    doSearch(Query.fromStrings(arg1, rel, arg2, corpora), filter, page, debug=debug, log=log)
  }

  def json(arg1: Option[String], rel: Option[String], arg2: Option[String], count: Int, corpora: Option[String]) = Action {
    import ExtractionGroupProtocol._
    Ok(tojson(Executor.executeRaw(Query.fromStrings(arg1, rel, arg2, corpora).toLowerCase).take(count)).toString.replaceAll("[\\p{C}]",""))
  }

  def instancesJson() = Action { implicit request =>
    Ok(Html("""<html><head><title>Instance Deserializer</title></head><body><h1>Instance Deserializer</h1><form method="POST"><textarea cols="80" rows="20" name="base64"></textarea><br /><input type="submit" /></body></html>"""))
  }

  case class InstanceInput(base64: String)
  val instanceForm = Form((mapping("base64" -> text)(InstanceInput.apply)(InstanceInput.unapply)))
  def instancesJsonSubmit() = Action { implicit request =>
    val input = instanceForm.bindFromRequest().get
    val base64 = input.base64

    import InstanceProtocol._
    val kryo = Chill.createInjection()
    val bytes = Base64.decodeBase64(base64)
    val instances = kryo.invert(bytes).get.asInstanceOf[List[Instance[Extraction]]]
    Ok(tojson(instances.head).toString.replaceAll("[\\p{C}]",""))
  }

  def sentences(arg1: Option[String], rel: Option[String], arg2: Option[String], title: String, debug: Boolean, corpora: Option[String]) = Action {
    val query = Query.fromStrings(arg1, rel, arg2, corpora)
    Logger.info("Showing sentences for title " + title + " in " + query)
    val group = searchGroups(query, debug)._1.groups.find(_.title.text == title) match {
      case None => throw new IllegalArgumentException("could not find group title: " + title)
      case Some(group) => group
    }

    Ok(views.html.sentences(group, debug))
  }

  def logsFromDate(date: DateTime = DateTime.now) =
    logs(date.getYear, date.getMonthOfYear, date.getDayOfMonth)

  def logs(year: Int, month: Int, day: Int) = Action {
    val today = new DateTime(year, month, day, 0, 0, 0, 0)

    Ok(views.html.logs(LogEntry.logs(year, month, day), today))
  }

  def searchGroups(query: Query, debug: Boolean) = {
    Logger.debug("incoming " + query)
    Cache.getAs[AnswerSet](query.toString.toLowerCase) match {
      case Some(answers) =>
        Logger.debug("retrieving " + query + " from cache")

        val AnswerSet(groups, filters, entities) = answers

        // cache hit
        Logger.info(query.toString +
          " retrieved from cache" +
          " with " + groups.size + " answers" +
          " and " + groups.iterator.map(_.contents.size).sum + " results")
        (answers, Some("cached"))
      case None =>
        Logger.debug("executing " + query + " in lucene")

        // cache miss
        val (ns, result) = Timing.time(Executor.execute(query.toLowerCase))

        val (groups, message) = result match {
          case Executor.Success(groups) => (groups, None)
          case Executor.Timeout(groups) => (groups, Some("timeout"))
          case Executor.Limited(groups) => (groups, Some("results truncated"))
        }

        val answers = AnswerSet.from(query, groups, TypeFilters.fromGroups(query, groups, debug))

        Logger.info(query.toString +
          " executed in " + Timing.Seconds.format(ns) +
          " with " + groups.size + " answers" +
          " and " + groups.iterator.map(_.contents.size).sum + " sentences" + message.map(" (" + _ + ")").getOrElse(""))

        // cache unless we had a timeout
        if (!result.isInstanceOf[Executor.Timeout[_]]) {
          Logger.debug("Saving " + query.toString + " to cache.")
          Cache.set(query.toString.toLowerCase, answers, 60 * 10)
        }

        (answers, message)
    }
  }

  def results(arg1: Option[String], rel: Option[String], arg2: Option[String], filterString: String, pageNumber: Int, debug: Boolean = false, corpora: Option[String]) = Action { implicit request =>
    doSearch(Query.fromStrings(arg1, rel, arg2, corpora), filterString, pageNumber, debug=debug, log=true, justResults=true)
  }

  def doSearch(query: Query, filterString: String, pageNumber: Int, debug: Boolean = false, log: Boolean = true, justResults: Boolean = false)(implicit request: RequestHeader) = {
    println(debug)
    val maxQueryTime = 20 * 1000 /* ms */

    val answers = scala.concurrent.future {
      searchGroups(query, debug)
    }

    Async {
      answers.map { case (answers, message) =>
          val filters: Set[TypeFilter] = filterString match {
            case "" | "all" => Set()
            case "misc" => answers.filters.map(_.filter).collect { case filter: PositiveTypeFilter => filter } .map(filter => NegativeTypeFilter(filter.typ, query.freeParts)).toSet
            case s => Set(PositiveTypeFilter(FreeBaseType.parse(s).getOrElse(throw new IllegalArgumentException("invalid type string: " + s)), query.freeParts))
          }

          val filtered = answers filter filters
          Logger.info(query + " with " + filters + " has " + filtered.answerCount + " answers " + filtered.sentenceCount + " results")
          val page = filtered.page(pageNumber, PAGE_SIZE)

          if (log) {
            val entry = LogEntry.fromRequest(query, filterString, answers.answerCount, answers.sentenceCount, request)
            entry.log()
          }

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
