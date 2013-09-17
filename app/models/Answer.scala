package models

import scala.util.control.Exception
import scala.Option.option2Iterable
import edu.knowitall.openie.models.{ReVerbExtraction, FreeBaseType, FreeBaseEntity, ExtractionGroup}
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.common.enrich.Traversables._
import edu.knowitall.common.enrich.Traversables.traversableOncePairIntTo
import edu.knowitall.common.Timing
import play.api.Logger
import edu.knowitall.openie.models.ExtractionCluster
import edu.knowitall.openie.models.Extraction

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
  
  def comesFromArg: Boolean = attrs.exists(_.contains("arg"))
  
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
  
  def contentsByRelation = contents.groupBy(_.rel).toList.sortBy{ case (r, cs) => -cs.size }
}

/** The Content class stores source information for a particular Answer. */
@SerialVersionUID(45L)
case class Content(strings: List[String], url: String, intervals: List[Interval], rel: String, confidence: Double, corpus: String) {
  
  def this(extr: Extraction) = {
    this(extr.sentenceTokens.toList map (_.string) map TripleQuery.clean,
         extr.source,
         List(extr.arg1Interval, extr.arg2Interval),
         extr.relText,
         extr.confidence,
         extr.corpus)
  }
  
  def sentence = strings.mkString(" ")
}

object Answer {
  
  def fromExtractionGroups(regs: Iterable[ExtractionCluster[Extraction]],
      group: ExtractionCluster[Extraction]=>Seq[AnswerPart],
      fullParts: Set[String]): Seq[Answer] = {

    val groups = Timing.timeThen {
      val splitUp = regs.iterator map (reg => ((group(reg), reg.instances.size), reg))
      val grouped = splitUp.toList.groupBy { case ((parts, size), reg) =>

        // hack: remove trailing 's'
        val text = parts.iterator.map(_.groupKey).mkString(", ")
        if (text.endsWith("s")) text.dropRight(1)
        else if (text.endsWith("es")) text.dropRight(2)
        else text
      }
      
      grouped.toList.sortBy { case (text, list) =>
        -list.iterator.map { case (title, reg) =>
          reg.instances.size
        }.sum
      }.map(_._2)
    } { ns =>
      Logger.debug("Group by lemmas in: " + Timing.Seconds.format(ns))
    }

    // organize extraction groups by a title (group by answer)
    val collapsed: Seq[(Seq[AnswerPart], Iterable[ExtractionCluster[Extraction]])] =
      Timing.timeThen {
        groups.map { list =>
            val ((headParts, _), _) = list.head

            // safe because of our groupBy
            val length = headParts.length
            val synonyms: Array[Seq[String]] = Array.fill(length)(Seq.empty)
            val entities: Array[Option[FreeBaseEntity]] = Array.fill(length)(None)
            val types: Array[Seq[FreeBaseType]] = Array.fill(length)(Seq.empty)

            val parts = for (i <- 0 until length) yield {
              // get all synonyms for the groups in this answer
              val synonyms: Seq[String] =
                list.flatMap(_._1._1(i).synonyms)(scala.collection.breakOut)

              // find the largest entity from the groups in this answer
              val entities: Option[FreeBaseEntity] = list.flatMap {
                case ((parts, size), _) =>
                  // turn the entities into a tuple with the groups' size
                  parts(i).entity.map((_, size))
              } match {
                case Nil => None
                // group like entities and add their sizes.  We might have multiple
                // groups that have the same entity and fit into the same answer.
                // Take the entity that has the largest size.
                case entities => Some(entities.mergeHistograms.maxBy(_._2)._1)
              }

              // lookup the first group that is linked to the entity we found,
              // and use the types from that group.
              val linkedTypes: Set[FreeBaseType] =
                entities.flatMap(entity => list.find {
                  case ((parts, size), _) =>
                    parts(i).entity == Some(entity)
                }).map {
                  case ((parts, size), _) =>
                    parts(i).types
                }.getOrElse(Set.empty)

              // for unlinkable type predictions, take types from the first group that wasn't linked but had types
              def unlinkableTypes = list.find({
                case ((parts, size), _) =>
                  parts(i).entity.isEmpty && !parts(i).types.isEmpty
              }).map {
                case ((parts, size), _) =>
                  parts(i).types
              }.getOrElse(Set.empty)

              // combine and remove base and user types
              val types = (if (!linkedTypes.isEmpty) linkedTypes else unlinkableTypes).filter(typ => typ.domain != "base" && typ.domain != "user")

              // group the synonyms and order them by size, descending
              val sortedUniqueSynonyms =
                synonyms.groupBy(_.toLowerCase).toList.map {
                  case (name, synonyms) =>
                    (synonyms.head, synonyms.size)
                }.sortBy(-_._2).map(_._1)

              AnswerPart(headParts(i).lemma.replaceAll("\t", " ").replaceAll("[\\p{C}]", ""),
                headParts(i).attrs,
                sortedUniqueSynonyms, entities, types)
            }

            (parts, list.map(_._2))
        }
    } { ns => Logger.debug("Collapsing groups in: " + Timing.Seconds.format(ns))}

    // convert to Content
    Timing.timeThen {
      collapsed.map {
        case (title, regs) =>
          val extractions = (regs flatMap (c => c.instances)).toList sortBy (-_.confidence)
          val list = extractions.map(e => new Content(e))

          // The answer discards information about the extractions from the
          // full part of the query.  However,
          val queryEntity: List[(FreeBaseEntity, Int)] = fullParts.toList match {
            case part :: Nil =>
              val entities: Iterable[(FreeBaseEntity, Int)] = regs.flatMap { group =>
                part match {
                  // map each option[entity] to option[entity, # of its entity]
                  case "r0.arg1" => group.arg1.entity.map((_, group.instances.size))
                  case "r0.rel" => group.arg2.entity.map((_, group.instances.size))
                  case "r0.arg2" => group.rel.entity.map((_, group.instances.size))
                }
              }

              // collapse entities with different scores together
              // use the score from the best-linked entity
              val collapsedEntities = entities.groupBy(_._1.fbid).toList.map { case (fbid, entities) =>
                // use the highest score entity for each entity
                val entity = entities.maxBy{ case (entity, count) => entity.score }._1
                // sum the count of the entities
                val count = entities.iterator.map { case (entity, count) => count }.sum
                (entity, count)
              }

              // sum the number of counts for each entity, and return a list
              // of (entity, # of that entity)
              collapsedEntities.mergeHistograms.toList.sortBy(_._2)(Ordering[Int].reverse)
            case _ => Nil
          }

          Answer(title, list, queryEntity)
      }.filter(_.contents.size > 0).sortBy(-_.contents.size)
    } { ns => Logger.debug("Convert to content in: " + Timing.Seconds.format(ns)) }
  }
}
