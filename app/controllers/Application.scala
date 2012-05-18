package controllers

import models.{Query, Group, LogEntry, AnswerSet, PositiveTypeFilter, NegativeTypeFilter, TypeFilter, TypeFilters}
import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.cache.Cache
import play.api.Play.current

import edu.washington.cs.knowitall.browser.lucene.ExtractionGroupFetcher
import edu.washington.cs.knowitall.browser.extraction.FreeBaseType

import edu.washington.cs.knowitall.common.Timing
import edu.washington.cs.knowitall.common.Resource.using

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

  def search(arg1: Option[String], rel: Option[String], arg2: Option[String], filter: String, page: Int) = Action {
    doSearch(Query.fromStrings(arg1, rel, arg2), filter, page)
  }

  def sentences(arg1: Option[String], rel: Option[String], arg2: Option[String], title: String) = Action {
    val query = Query.fromStrings(arg1, rel, arg2)
    Logger.info("Showing sentences for title " + title + " in " + query)
    val group = searchGroups(query).groups.find(_.title.text == title) match {
      case None => throw new IllegalArgumentException("could not find group title: " + title)
      case Some(group) => group
    }

    Ok(views.html.sentences(group))
  }

  def logs(year: Int, month: Int, day: Int) = Action {
    Ok(views.html.logs(LogEntry.logs(year, month, day)))
  }

  def searchGroups(query: Query) = {
    Cache.getAs[AnswerSet](query.toString) match {
      case Some(answers) =>
        val AnswerSet(groups, filters) = answers

        // cache hit
        Logger.info(query.toString +
          " retrieved from cache" +
          " with " + groups.size + " groups" +
          " and " + groups.iterator.map(_.contents.size).sum + " results")
        answers
      case None =>
        // cache miss
        val (ns, groups) = Timing.time(query.execute())
        Logger.info(query.toString +
          " executed in " + Timing.Seconds.format(ns) +
          " with " + groups.size + " answers" +
          " and " + groups.iterator.map(_.contents.size).sum + " sentences")
        val answers = AnswerSet.from(groups, TypeFilters.fromGroups(groups))
        Cache.set(query.toString, answers)
        answers
    }
  }

  def doSearch(query: Query, filterString: String, pageNumber: Int) = {
    val answers = searchGroups(query)

    val filters = filterString match {
      case "" | "all" => Set()
      case "other" => answers.filters.map(filter => NegativeTypeFilter(filter._1.typ))
      case s => println(s); Set(PositiveTypeFilter(FreeBaseType.parse(s).getOrElse(throw new IllegalArgumentException("invalid type string: " + s))))
    }

    val filtered = answers filter filters
    Logger.info(query + " with " + filters + " has " + filtered.answerCount + " answers " + filtered.sentenceCount + " results")
    val page = filtered.page(pageNumber, PAGE_SIZE)

    val entry = new LogEntry(query, answers.answerCount, answers.sentenceCount)
    entry.log()

    Ok(views.html.results(searchForm, query, page, filtered.answerCount, filtered.sentenceCount, filters.toSet, pageNumber, math.ceil(filtered.answerCount.toDouble / PAGE_SIZE.toDouble).toInt, MAX_SENTENCE_COUNT))
  }
}
