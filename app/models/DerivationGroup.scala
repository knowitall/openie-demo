package models

import edu.knowitall.collection.immutable.Interval
import edu.knowitall.paraphrasing.{Paraphrase, IdentityDerivation, ScoredParaphraseDerivation}

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

case class DerivationGroup(val interpretation: String, val paraphrases : Seq[Paraphrase], val queryTriples: Seq[(String, Seq[Triple])]) {
  def resultsCount = queryTriples.flatMap(_._2).size
}

object DerivationGroup {

  def ppDerivationSort(pp: Paraphrase): Double = {
    pp.derivation match {
      case IdentityDerivation => Int.MinValue
      case _ => -pp.derivation.score
    }
  }

  def regroup(dgroups: Seq[DerivationGroup]): Seq[DerivationGroup] = {

    // get derivation groups sharing the same equery
    val queryGroups = dgroups.groupBy(_.interpretation)
    // break out combined paraphrases for these derivations
    val ppGroups = queryGroups.iterator.toSeq.map { case (interp, dgs) => (interp, dgs.flatMap(_.paraphrases).distinct, dgs) }
    // merge queryTriples for each and done.
    val regrouped = ppGroups.iterator.toSeq.map { case (interp, pps, dgs) =>
      val allQueryTriples = dgs.flatMap(_.queryTriples)
      val regroupedQTs = allQueryTriples.groupBy(_._1).map(p => (p._1, p._2.flatMap(_._2))).iterator.toSeq
      DerivationGroup(interp, pps.sortBy(ppDerivationSort), regroupedQTs.filterNot(_._2.isEmpty))
    }
    regrouped.sortBy(dg => ppDerivationSort(dg.paraphrases.head))
  }


  def dedupe(dgroups: Seq[DerivationGroup]): Seq[DerivationGroup] = {

    dgroups.map { case d @ DerivationGroup(_, _, queryTriples) =>
      val deduped = queryTriples.map { case (query, triples) => (query, triples.distinct) }
      d.copy(queryTriples = deduped)
    }
  }
}