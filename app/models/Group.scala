package models

import java.util.regex.Pattern

import scala.collection.JavaConverters._

import edu.washington.cs.knowitall.collection.immutable.Interval
import edu.washington.cs.knowitall.common.enrich.Traversables._
import edu.washington.cs.knowitall.commonlib.Range

import edu.washington.cs.knowitall.browser.lucene.ExtractionGroupFetcher
import edu.washington.cs.knowitall.browser.extraction.ExtractionGroup
import edu.washington.cs.knowitall.browser.extraction.ReVerbExtraction
import edu.washington.cs.knowitall.browser.extraction.Instance
import edu.washington.cs.knowitall.browser.extraction.FreeBaseEntity
import edu.washington.cs.knowitall.browser.extraction.FreeBaseType

@SerialVersionUID(42L)
case class GroupTitlePart(lemma: String, synonyms: Seq[String], entity: Option[FreeBaseEntity], types: Seq[FreeBaseType]) {
  def text = entity match {
    case Some(entity) => entity.name
    case None => lemma
  }
}

@SerialVersionUID(43L)
case class GroupTitle(connector: String, parts: Seq[GroupTitlePart]) {
  def text: String = parts.iterator.map(_.text).mkString(connector)
}

@SerialVersionUID(44L)
case class Group(title: GroupTitle, contents: List[Content])

@SerialVersionUID(45L)
case class Content(strings: List[String], url: String, intervals: List[Interval]) {
  def sentence = strings.mkString(" ")
}

object Group {
  def fromExtractionGroups(reg: Iterator[ExtractionGroup[ReVerbExtraction]],
      group: ExtractionGroup[ReVerbExtraction]=>GroupTitle): Seq[Group] = {
    def rangeToInterval(range: Range) = {
      Interval.open(range.getStart, range.getEnd)
    }

    val groups = ((reg map (reg => (group(reg), reg))).toList groupBy { case (title, reg) => title.parts.map(_.lemma.toLowerCase) }).toList.
      sortBy { case (text, list) => -list.iterator.map { case (title, reg) => reg.instances.size }.sum }

    val collapsed: Seq[(GroupTitle, Iterable[ExtractionGroup[ReVerbExtraction]])] = groups.map { case (text, list) =>
      val (headTitle, _) = list.head

      // safe because of our groupBy
      val length = headTitle.parts.length
      var synonyms: Array[Seq[String]] = Array.fill(length)(Seq.empty)
      var entities: Array[Option[FreeBaseEntity]] = Array.fill(length)(None)
      var types: Array[Seq[FreeBaseType]] = Array.fill(length)(Seq.empty)
      val parts = for (i <- 0 until length) yield {
        val synonyms: Seq[String] =
          list.flatMap(_._1.parts(i).synonyms)(scala.collection.breakOut)

        val entities: Option[FreeBaseEntity] =
          list.flatMap(_._1.parts(i).entity).headOption

        val types: Seq[FreeBaseType] =
          list.flatMap(_._1.parts(i).types)(scala.collection.breakOut)

        val sortedUniqueSynonyms =
          synonyms.groupBy(_.toLowerCase).toList.map { case (name, synonyms) =>
            (synonyms.head, synonyms.size)
          }.sortBy(- _._2).map(_._1)

        GroupTitlePart(sortedUniqueSynonyms.headOption.getOrElse(headTitle.parts(i).lemma), sortedUniqueSynonyms, entities, types)
      }

      val title = GroupTitle(headTitle.connector, parts)
      (title, list.map(_._2))
    }

    collapsed.map { case (title, contents) =>
      val instances = (contents flatMap (_.instances)).toList sortBy (- _.confidence)
      val list = instances.map { instance =>
        val sentence = instance.extraction.source.getSentence.getTokens
        val url = instance.extraction.sourceUrl
        val ranges = List(
            instance.extraction.source.getArgument1.getRange,
            instance.extraction.source.getRelation.getRange,
            instance.extraction.source.getArgument2.getRange)
        Content(sentence.asScala.toList.map(Query.clean), url, ranges map rangeToInterval)
      }.toList

      Group(title, list)
    }.filter(_.contents.size > 0)

    .sortBy(-_.contents.size)
  }
}
