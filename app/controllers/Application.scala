package controllers

import play.api._
import play.api.mvc._
import models.Query
import play.api.data._
import play.api.data.Forms._

object Application extends Controller {
  /**
    * The actual definition of the search form.
    */
  val searchForm: Form[Query] = Form(
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
      query => Ok(views.html.results(query.arg1.getOrElse(""), query.rel.getOrElse(""), query.arg2.getOrElse(""))))
  }

  /**
    * Search by specifying params.  Used for GET requests.
    */
  def searchParams(arg1: Option[String], rel: Option[String], arg2: Option[String]) = {
    this.search(Query(arg1, rel, arg2))
  }

  /**
    * Search with specified query.  Used for POST requests.
    */
  def search(query: Query) = Action {
    Ok(views.html.results(query.arg1.getOrElse(""), query.rel.getOrElse(""), query.arg2.getOrElse("")))
  }
}