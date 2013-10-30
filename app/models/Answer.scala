package models

import scala.util.control.Exception
import scala.Option.option2Iterable
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.common.enrich.Traversables._
import edu.knowitall.common.enrich.Traversables.traversableOncePairIntTo
import edu.knowitall.common.Timing
import play.api.Logger

/** An Answer can have multiple parts, each being linked to a different entity. */
@SerialVersionUID(42L)
case class AnswerPart(lemma: String, attrs: Set[String], synonyms: Seq[String], entity: Option[FreeBaseEntity], types: Seq[AbstractType]) {
  // The entity name if linked, else the first synonym if available, else the lemma
  // (Although there should probably always be a synonym)
  def text = entity.map(_.name).getOrElse(synonyms.headOption.getOrElse(lemma))

  def fbTypes = types.collect { case f: FreeBaseType => f }

  // The entity id, if linked, else the lemma
  def groupKey = entity.map(_.fbid).getOrElse(lemma).toLowerCase.trim

  /** Show synonyms other than the text of this part */
  def otherSynonyms = synonyms filterNot (_ equalsIgnoreCase text)

  def comesFromArg1: Boolean = attrs.exists(_.contains("arg1"))

  def comesFromArg2: Boolean = attrs.exists(_.contains("arg2"))

  def comesFromRel: Boolean = attrs.exists(_.contains("rel"))
}

/** The Answer class represents a result in the Answer pane.
  *
  * @param  title  the title of this answer
  * @param  contents  the extractions and sources of this answer
  * @param  queryEntity  the entity for these extractions in the singular full section of the query
  */
@SerialVersionUID(44L)
case class Answer(parts: Seq[AnswerPart], dgroups: Seq[DerivationGroup], queryEntity: List[(FreeBaseEntity, Int)]) {

  def title = parts.map(_.text).mkString(", ")

  def arg1Full = parts.forall(_.comesFromArg1)
  def relFull  = parts.forall(_.comesFromRel)
  def arg2Full = parts.forall(_.comesFromArg2)

  def resultsCount = dgroups.map(_.resultsCount).sum

  val attrs = parts.flatMap(_.attrs).toSet

  val allTriples = dgroups.flatMap(_.queryTriples.flatMap(_._2))

  val allTriplesByRel = allTriples.groupBy(_.rel)

//  def limitResults(max: Int): Answer = {
//    // expand dgroups and limit then regroup
//    val expanded = dgroups.flatMap { case DerivationGroup(interp, pps, queryTriples) =>
//      queryTriples.flatMap { case (query, triples) =>
//        triples.map(t => (pps, query, t))
//      }
//    }
//    val limited = expanded.take(max)
//    val regrouped = limited.groupBy(_._1).iterator.toSeq.map { case (pps, group) =>
//      val queryTriples = group.groupBy(_._2).iterator.toSeq.map { case (query, group2) =>
//        (query, group2.map(_._3))
//      }.toSeq
//      DerivationGroup(pps, queryTriples)
//    }
//    this.copy(dgroups = regrouped)
//  }
//
//  def limitSentences(max: Int): Answer = {
//    val truncatedDgroups = dgroups.map { dg =>
//      val truncatedTriples = dg.queryTriples.map { case (query, triples) =>
//        (query, triples.take(max))
//      }
//      dg.copy(queryTriples = truncatedTriples)
//    }
//    this.copy(dgroups = truncatedDgroups)
//  }
}
