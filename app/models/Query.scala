package models

case class Query(
  arg1: Option[String],
  rel: Option[String],
  arg2: Option[String]) {

  require(arg1.isDefined ||
    rel.isDefined ||
    arg2.isDefined,
    "At least one relation part must be specified.")

  def this(arg1: String, rel: String, arg2: String) =
    this(Some(arg1), Some(rel), Some(arg2))
}