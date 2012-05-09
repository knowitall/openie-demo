package controllers

import models.{Query, Group}
import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.cache.Cache
import play.api.Play.current

import edu.washington.cs.knowitall.browser.lucene.ExtractionGroupFetcher

import edu.washington.cs.knowitall.common.Timing

object Application extends Controller {
  final val PAGE_SIZE = 100
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
      query => {
        val groups = Cache.getAs[List[Group]](query.toString) match {
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
              " executed in " + Timing.Seconds.format(ns) + 
              " with " + groups.size + " groups" + 
              " and " + groups.iterator.map(_.contents.size).sum + " results")
            Cache.set(query.toString, groups)
            groups
        }

        // val page = groups.drop(query.page * PAGE_SIZE).take(PAGE_SIZE)

        Ok(views.html.results(searchForm, query, groups))
      })
  }
}
