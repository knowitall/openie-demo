package models

import scala.Option.option2Iterable

import edu.washington.cs.knowitall.browser.extraction.{ReVerbExtraction, FreeBaseType, FreeBaseEntity, ExtractionGroup}
import edu.washington.cs.knowitall.collection.immutable.Interval
import edu.washington.cs.knowitall.common.enrich.Traversables.traversableOncePairIntTo

@SerialVersionUID(42L)
case class GroupTitlePart(lemma: String, extractionPart: ExtractionPart, synonyms: Seq[String], entity: Option[FreeBaseEntity], types: Set[FreeBaseType]) {
  def text = entity match {
    case Some(entity) => entity.name
    case None => synonyms.headOption.getOrElse(lemma)
  }

  /** Show synonyms other than the text of this part */
  def otherSynonyms = synonyms filterNot (_ equalsIgnoreCase text)
}

@SerialVersionUID(43L)
case class GroupTitle(connector: String, parts: Seq[GroupTitlePart]) {
  def text: String = parts.iterator.map(_.text).mkString(connector)
}

@SerialVersionUID(44L)
case class Group(title: GroupTitle, contents: List[Content])

@SerialVersionUID(45L)
case class Content(strings: List[String], url: String, intervals: List[Interval], confidence: Double) {
  def sentence = strings.mkString(" ")
}

object Group {
  def fromExtractionGroups(reg: Iterable[ExtractionGroup[ReVerbExtraction]],
      group: ExtractionGroup[ReVerbExtraction]=>GroupTitle): Seq[Group] = {

    val groups = ((reg map (reg => ((group(reg), reg.instances.size), reg))).toList groupBy { case ((title, size), reg) =>
        def partText(part: GroupTitlePart) = part.entity match {
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

    val collapsed: Seq[(GroupTitle, Iterable[ExtractionGroup[ReVerbExtraction]])] = groups.map { case (text, list) =>
      val ((headTitle, headTitleSize), _) = list.head

      // safe because of our groupBy
      val length = headTitle.parts.length
      var synonyms: Array[Seq[String]] = Array.fill(length)(Seq.empty)
      var entities: Array[Option[FreeBaseEntity]] = Array.fill(length)(None)
      var types: Array[Seq[FreeBaseType]] = Array.fill(length)(Seq.empty)
      val parts = for (i <- 0 until length) yield {
        val synonyms: Seq[String] =
          list.flatMap(_._1._1.parts(i).synonyms)(scala.collection.breakOut)

        val entities: Option[FreeBaseEntity] = list.flatMap { case ((title, size), _) => title.parts(i).entity.map((_, size)) } match {
          case Nil => None
          case entities => Some(entities.mergeHistograms.maxBy(_._2)._1)
        }

        val types: Set[FreeBaseType] =
          entities.flatMap(entity => list.find { case((title, size), _) =>
            title.parts(i).entity == Some(entity)
          }).map { case ((title, size), _) =>
            title.parts(i).types
          }.getOrElse(Set.empty).filter(typ => typ.domain != "base" && typ.domain != "user")

        val sortedUniqueSynonyms =
          synonyms.groupBy(_.toLowerCase).toList.map { case (name, synonyms) =>
            (synonyms.head, synonyms.size)
          }.sortBy(- _._2).map(_._1)

          GroupTitlePart(headTitle.parts(i).lemma,
              headTitle.parts(i).extractionPart,
              sortedUniqueSynonyms, entities, types)
        }

      val title = GroupTitle(headTitle.connector, parts)
      (title, list.map(_._2))
    }

    collapsed.map { case (title, contents) =>
      val instances = (contents flatMap (c => c.instances.map((c.arg1Entity, _)))).toList sortBy (- _._2.confidence)
      val list = instances.map { case (e, instance) =>
        val sentence = instance.extraction.sentenceTokens.map(_.string)
        val url = instance.extraction.sourceUrl
        val intervals = List(
            instance.extraction.arg1Interval,
            instance.extraction.relInterval,
            instance.extraction.arg2Interval)
        Content(sentence.toList.map(Query.clean), url, intervals, instance.confidence)
      }.toList

      Group(title, list)
    }.filter(_.contents.size > 0).sortBy(-_.contents.size)
  }
}
