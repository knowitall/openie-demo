package models

import edu.knowitall.collection.immutable.Interval
import edu.knowitall.paraphrasing.Paraphrase

/**
 * Represents an individual Triple and associated metadata.
 */
sealed trait Triple {

  def arg1: String

  def rel: String

  def arg2: String

  /**
   * The name, e.g. "NELL", "FreeBase", "Open IE 4"
   */
  def source: String
  /**
   * A link to justify the triple, e.g. link to NELL page, Freebase, a webpage containing Open IE extracted text, etc.
   */
  def url: String
}
case class DefaultTriple(val arg1: String, val rel: String, val arg2: String, val source: String, val url: String) extends Triple
case class OpenIETriple(
    val sentenceStrings: Seq[String],
    val arg1Interval: Interval,
    val relInterval: Interval,
    val arg2Interval: Interval,
    val confidence: Double,
    val corpus: String,
    val source: String,
    val url: String) extends Triple {

  def sentenceSlice(i: Interval) = sentenceStrings.slice(i.start, i.end).mkString(" ")

  def boldIntervals = Seq(arg1Interval, arg2Interval)

  def arg1 = sentenceSlice(arg1Interval)
  def rel = sentenceSlice(relInterval)
  def arg2 = sentenceSlice(arg2Interval)
}

case class DerivationGroup(val paraphrases : Seq[Paraphrase], val queryTriples: Seq[(String, Seq[Triple])]) {
  def resultsCount = queryTriples.flatMap(_._2).size
}

object DerivationGroup {

  import edu.knowitall.apps.AnswerDerivation

  def from(derivs: Seq[AnswerDerivation]): Seq[DerivationGroup] = {
    Nil
  }

}