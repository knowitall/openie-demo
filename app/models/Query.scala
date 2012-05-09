package models

import edu.washington.cs.knowitall.browser.lucene.ExtractionGroupFetcher
import edu.washington.cs.knowitall.browser.extraction.ExtractionGroup
import edu.washington.cs.knowitall.browser.extraction.ReVerbExtraction

object Query {
  type REG = ExtractionGroup[ReVerbExtraction]
}

case class Query(
  arg1: Option[String],
  rel: Option[String],
  arg2: Option[String]) {

  import Query._

  require(arg1.isDefined ||
    rel.isDefined ||
    arg2.isDefined,
    "At least one relation part must be specified.")

  def this(arg1: String, rel: String, arg2: String) =
    this(Some(arg1), Some(rel), Some(arg2))

  def arg1String = arg1.getOrElse("")
  def relString = rel.getOrElse("")
  def arg2String = arg2.getOrElse("")

  def execute() = {
    def group: REG=>String = (this.arg1, this.rel, this.arg2) match {
      case (Some(arg1), None, None) => (eg: REG) => eg.relNorm + " " + eg.arg2Norm
      case (None, Some(rel), None) => (eg: REG) => eg.arg1Norm + ", " + eg.arg2Norm
      case (None, None, Some(arg2)) => (eg: REG) => eg.arg1Norm + " " + eg.relNorm

      case (Some(arg1), Some(rel), None) => (eg: REG) => eg.arg2Norm
      case (None, Some(rel), Some(arg2)) => (eg: REG) => eg.arg1Norm
      case (Some(arg1), None, Some(arg2)) => (eg: REG) => eg.relNorm

      case (Some(arg1), Some(rel), Some(arg2)) => (eg: REG) => eg.arg2Norm
      case (None, None, None) => (eg: REG) => eg.arg1Norm + " " + eg.relNorm + " " + eg.arg2Norm
    }

    val indexPath = "/scratch/common/browserplus/test-index"
    val fetcher = new ExtractionGroupFetcher(indexPath)
    val results = fetcher.
        getGroups(this.arg1.getOrElse(""), this.rel.getOrElse(""), this.arg2.getOrElse(""))
    val groups = Group.fromExtractionGroups(results, group)

    groups
  }
}
