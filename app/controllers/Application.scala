package controllers

import models.{Query, Group, LogEntry}
import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.cache.Cache
import play.api.Play.current

import edu.washington.cs.knowitall.browser.lucene.ExtractionGroupFetcher

import edu.washington.cs.knowitall.common.Timing

object Application extends Controller {
  final val PAGE_SIZE = 50
  final val MAX_SENTENCE_COUNT = 50

  /**
    * The actual definition of the search form.
    */
  def searchForm: Form[Query] = Form(
    // Defines a mapping that will handle Contact values
    mapping(
      "arg1" -> optional(text),
      "rel" -> optional(text),
      "arg2" -> optional(text))(Query.apply)(Query.unapply))

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
      query => doSearch(query, 0))
  }

  def search(arg1: Option[String], rel: Option[String], arg2: Option[String], page: Int) = Action {
    doSearch(Query(arg1, rel, arg2), page)
  }

  def sentences(arg1: Option[String], rel: Option[String], arg2: Option[String], title: String) = Action {
    val group = searchGroups(Query(arg1, rel, arg2)).find(_.title == title) match {
      case None => throw new IllegalArgumentException("could not find group title: " + title)
      case Some(group) => group
    }

    println(group.contents.mkString("\n"))

    Ok(views.html.sentences(group))
  }

  def searchGroups(query: Query) = {
    Cache.getAs[List[Group]](query.toString) match {
      case Some(groups) =>
        // cache hit
        Logger.info(query.toString +
          " retrieved from cache" +
          " with " + groups.size + " groups" +
          " and " + groups.iterator.map(_.contents.size).sum + " results")
        groups
      case None =>
        // cache miss
        val (ns, groups) = Timing.time(query.execute())
        Logger.info(query.toString +
          " from " +
          " executed in " + Timing.Seconds.format(ns) +
          " with " + groups.size + " groups" +
          " and " + groups.iterator.map(_.contents.size).sum + " results")
        Cache.set(query.toString, groups)
        groups
    }
  }

  def doSearch(query: Query, pageNumber: Int) = {
    val groups = searchGroups(query)
    val page = groups.drop(pageNumber * PAGE_SIZE).take(PAGE_SIZE)

    val entry = new LogEntry(query, groups.size, groups.map(_.contents.size).sum)
    entry.log()

    Ok(views.html.results(searchForm, query, page, pageNumber, math.ceil(groups.size.toDouble / PAGE_SIZE.toDouble).toInt, MAX_SENTENCE_COUNT))
  }
}
