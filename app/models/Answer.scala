package models

import scala.util.control.Exception
import scala.Option.option2Iterable
import edu.knowitall.openie.models.{ReVerbExtraction, FreeBaseType, FreeBaseEntity, ExtractionGroup}
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.common.enrich.Traversables._
import edu.knowitall.common.enrich.Traversables.traversableOncePairIntTo
import edu.knowitall.common.Timing
import play.api.Logger

/** An Answer can have multiple parts, each being linked to a different entity. */
@SerialVersionUID(42L)
case class AnswerPart(lemma: String, attrs: Set[String], synonyms: Seq[String], entity: Option[FreeBaseEntity], types: Set[FreeBaseType]) {
  // The entity name if linked, else the first synonym if available, else the lemma
  // (Although there should probably always be a synonym)
  def text = entity.map(_.name).getOrElse(synonyms.headOption.getOrElse(lemma))
  
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
case class Answer(parts: Seq[AnswerPart], contents: List[Content], queryEntity: List[(FreeBaseEntity, Int)]) {
  
  def title = parts.map(_.text).mkString(", ")
  
  def arg1Full = parts.forall(_.comesFromArg1)
  def relFull  = parts.forall(_.comesFromRel)
  def arg2Full = parts.forall(_.comesFromArg2)
  
  val attrs = parts.flatMap(_.attrs).toSet
  
  def contentsByRelation = contents.groupBy(_.rel).toList.sortBy{ case (r, cs) => -cs.size }
}

/** The Content class stores source information for a particular Answer. */
@SerialVersionUID(45L)
case class Content(strings: List[String], url: String, intervals: List[Interval], rel: String, confidence: Double, corpus: String) {
  
  def sentence = strings.mkString(" ")
}
