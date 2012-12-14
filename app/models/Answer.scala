package models

import scala.util.control.Exception
import scala.Option.option2Iterable

import edu.washington.cs.knowitall.browser.extraction.{ReVerbExtraction, FreeBaseType, FreeBaseEntity, ExtractionGroup}
import edu.washington.cs.knowitall.collection.immutable.Interval
import edu.washington.cs.knowitall.common.enrich.Traversables._
import edu.washington.cs.knowitall.common.enrich.Traversables.traversableOncePairIntTo

/** An Answer can have multiple parts, each being linked to a different entity. */
@SerialVersionUID(42L)
case class AnswerTitlePart(lemma: String, extractionPart: ExtractionPart, synonyms: Seq[String], entity: Option[FreeBaseEntity], types: Set[FreeBaseType]) {
  def text = entity match {
    case Some(entity) => entity.name
    case None => synonyms.headOption.getOrElse(lemma)
  }

  /** Show synonyms other than the text of this part */
  def otherSynonyms = synonyms filterNot (_ equalsIgnoreCase text)
}

/** The AnswerTitle class represents the title of an Answer. */
@SerialVersionUID(43L)
case class AnswerTitle(connector: String, parts: Seq[AnswerTitlePart]) {
  def text: String = parts.iterator.map(_.text).mkString(connector)
}

/** The Answer class represents a result in the Answer pane.
  *
  * @param  title  the title of this answer
  * @param  contents  the extractions and sources of this answer
  * @param  queryEntity  the entity for these extractions in the singular full section of the query
  */
@SerialVersionUID(44L)
case class Answer(title: AnswerTitle, contents: List[Content], queryEntity: Option[FreeBaseEntity]) {
  def contentsByRelation = contents.groupBy(_.rel).toList.sortBy{ case (r, cs) => -cs.size }
}

/** The Content class stores source information for a particular Answer. */
@SerialVersionUID(45L)
case class Content(strings: List[String], url: String, intervals: List[Interval], rel: String, confidence: Double, corpus: String) {
  def sentence = strings.mkString(" ")
}

object Answer {
  def fromExtractionGroups(reg: Iterable[ExtractionGroup[ReVerbExtraction]],
      group: ExtractionGroup[ReVerbExtraction]=>AnswerTitle,
      fullParts: List[ExtractionPart]): Seq[Answer] = {

    // create a title for each group
    // then group by the title lemma or the entity name
    val groups = ((reg map (reg => ((group(reg), reg.instances.size), reg))).toList groupBy { case ((title, size), reg) =>
        def partText(part: AnswerTitlePart) = part.entity match {
          case Some(entity) => entity.name
          case None => part.lemma
        }

        // hack: remove trailing 's'
        val text = title.parts.map(partText).mkString(title.connector).toLowerCase.trim
        if (text.endsWith("s")) text.dropRight(1)
        else if (text.endsWith("es")) text.dropRight(2)
        else text
      }).toList.sortBy { case (text, list) =>
        -list.iterator.map { case (title, reg) =>
          reg.instances.size
        }.sum
      }
    
    // create a title from the grouped ExtractionGroups
    // this involves determining what entities and types to keep
    val collapsed: Seq[(AnswerTitle, Iterable[ExtractionGroup[ReVerbExtraction]])] = groups.map { case (text, list) =>
      val ((headTitle, headTitleSize), _) = list.head

      // safe because of our groupBy
      val length = headTitle.parts.length
      var synonyms: Array[Seq[String]] = Array.fill(length)(Seq.empty)
      var entities: Array[Option[FreeBaseEntity]] = Array.fill(length)(None)
      var types: Array[Seq[FreeBaseType]] = Array.fill(length)(Seq.empty)

      val parts = for (i <- 0 until length) yield {
        // get all synonyms for the groups in this answer
        val synonyms: Seq[String] =
          list.flatMap(_._1._1.parts(i).synonyms)(scala.collection.breakOut)

        // find the largest entity from the groups in this answer
        val entities: Option[FreeBaseEntity] = list.flatMap { case ((title, size), _) =>
          // turn the entities into a tuple with the groups' size
          title.parts(i).entity.map((_, size)) } match {
            case Nil => None
            // group like entities and add their sizes.  We might have multiple
            // groups that have the same entity and fit into the same answer.
            // Take the entity that has the largest size.
            case entities => Some(entities.mergeHistograms.maxBy(_._2)._1)
          }

        // lookup the first group that is linked to the entity we found,
        // and use the types from that group.
        val linkedTypes: Set[FreeBaseType] =
          entities.flatMap(entity => list.find { case((title, size), _) =>
            title.parts(i).entity == Some(entity)
          }).map { case ((title, size), _) =>
            title.parts(i).types
          }.getOrElse(Set.empty)

        // for unlinkable type predictions, take types from the first group that wasn't linked but had types
        def unlinkableTypes = list.find({ case((title, size), _) =>
            title.parts(i).entity.isEmpty && !title.parts(i).types.isEmpty
        }).map { case ((title, size), _) =>
            title.parts(i).types
        }.getOrElse(Set.empty)

        // combine and remove base and user types
        val types = (if (!linkedTypes.isEmpty) linkedTypes else unlinkableTypes).filter(typ => typ.domain != "base" && typ.domain != "user")

        // group the synonyms and order them by size, descending
        val sortedUniqueSynonyms =
          synonyms.groupBy(_.toLowerCase).toList.map { case (name, synonyms) =>
            (synonyms.head, synonyms.size)
          }.sortBy(- _._2).map(_._1)

          AnswerTitlePart(headTitle.parts(i).lemma.replaceAll("\t", " ").replaceAll("[\\p{C}]",""),
              headTitle.parts(i).extractionPart,
              sortedUniqueSynonyms, entities, types)
        }

      val title = AnswerTitle(headTitle.connector, parts)
      (title, list.map(_._2))
    }

    // convert to Content
    collapsed.map { case (title, contents) =>
      val instances = (contents flatMap (c => c.instances.map((c, _)))).toList sortBy (- _._2.confidence)
      val list = instances.map { case (group, instance) =>
        //val sentence = instance.extraction.sentenceTokens.map(_.string)
        //val url = instance.extraction.sourceUrl
        val intervals = List(
            Interval.open(0, 1),
            Interval.open(0, 1),
            Interval.open(0, 1))
        Content(List("foo", "bar"), "url", intervals, "rel", 1.0, "corpus")
      }.toList

      // The answer discards information about the extractions from the
      // full part of the query.  However, it's needed for the browser
      // experience so we retain it here.
      val queryEntity = fullParts match {
        case part :: Nil =>
          val entities = instances.flatMap { inst =>
            part match {
              case Argument1 => inst._1.arg1.entity
              case Argument2 => inst._1.arg2.entity
              case Relation => inst._1.rel.entity
            }
          }

          // collapse entities with different scores together
          // use the score from the best-linked entity
          val collapsedEntities = entities.groupBy(_.fbid).toList.sortBy(_._2.size)(Ordering[Int].reverse).map { group =>
            group._2.maxBy(_.score)
          }

          Exception.allCatch opt collapsedEntities.histogram.maxBy(_._2)._1
        case _ => None
      }

      Answer(title, list, queryEntity)
    }.filter(_.contents.size > 0).sortBy(-_.contents.size)
  }
}
