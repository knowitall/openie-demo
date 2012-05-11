package models

import java.util.regex.Pattern

import scala.collection.JavaConverters._

import edu.washington.cs.knowitall.collection.immutable.Interval
import edu.washington.cs.knowitall.commonlib.Range

import edu.washington.cs.knowitall.browser.lucene.ExtractionGroupFetcher
import edu.washington.cs.knowitall.browser.extraction.ExtractionGroup
import edu.washington.cs.knowitall.browser.extraction.ReVerbExtraction
import edu.washington.cs.knowitall.browser.extraction.Instance
import edu.washington.cs.knowitall.browser.extraction.FreeBaseEntity
import edu.washington.cs.knowitall.browser.extraction.FreeBaseType

@SerialVersionUID(42L)
case class GroupTitlePart(lemma: String, synonyms: Seq[String], entity: Option[FreeBaseEntity], types: Seq[FreeBaseType])

@SerialVersionUID(43L)
case class GroupTitle(connector: String, parts: Seq[GroupTitlePart]) {
  def text: String = parts.iterator.map(_.lemma).mkString(connector)
}

@SerialVersionUID(44L)
case class Group(title: GroupTitle, contents: List[Content])

@SerialVersionUID(45L)
case class Content(strings: List[String], url: String, intervals: List[Interval]) {
  def sentence = strings.mkString(" ")
}

object Group {
  def fromExtractionGroups(reg: Iterable[ExtractionGroup[ReVerbExtraction]],
      group: ExtractionGroup[ReVerbExtraction]=>GroupTitle): Seq[Group] = {
    def rangeToInterval(range: Range) = {
      Interval.open(range.getStart, range.getEnd)
    }

    val groups = (reg map (reg => (group(reg), reg)) groupBy { case (title, reg) => title.text }).toList.
      sortBy { case (text, list) => -list.iterator.map { case (title, reg) => reg.instances.size }.sum }

    val collapsed: Seq[(GroupTitle, Iterable[ExtractionGroup[ReVerbExtraction]])] = groups.map { case (text, list) =>
      val (headTitle, _) = list.head
      val titles = list.map { case (title, reg) => title }
      val title = GroupTitle(headTitle.connector, headTitle.parts)
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
        Content(sentence.asScala.toList, url, ranges map rangeToInterval)
      }.toList

      Group(title, list)
    }.filter(_.contents.size > 0)
    
    .sortBy(-_.contents.size)
  }
}
