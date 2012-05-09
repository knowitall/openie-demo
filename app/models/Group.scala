package models

import java.util.regex.Pattern

import scala.collection.JavaConverters._

import edu.washington.cs.knowitall.collection.immutable.Interval
import edu.washington.cs.knowitall.commonlib.Range

import edu.washington.cs.knowitall.browser.lucene.ExtractionGroupFetcher
import edu.washington.cs.knowitall.browser.extraction.ExtractionGroup
import edu.washington.cs.knowitall.browser.extraction.ReVerbExtraction
import edu.washington.cs.knowitall.browser.extraction.Instance


case class Group(title: String, contents: List[Content])
case class Content(strings: List[String], url: String, intervals: List[Interval]) {
  def sentence = strings.mkString(" ")
}

object Group {
  def fromExtractionGroups(reg: Iterable[ExtractionGroup[ReVerbExtraction]],
      group: ExtractionGroup[ReVerbExtraction]=>String): List[Group] = {
    def rangeToInterval(range: Range) = {
      Interval.open(range.getStart, range.getEnd)
    }

    val groups = (reg groupBy group).toList sortBy (-_._2.iterator.map(_.instances.size).sum)
    groups.map { case (title, contents) =>
      val instances = contents.flatMap(_.instances).filter(Group.filterInstances)
      val list = instances.toList.sortBy { instance =>
        -instance.confidence.getOrElse(0.0)
      }.map { instance =>
        val sentence = instance.extraction.source.getSentence.getTokens
        val url = instance.extraction.sourceUrl.getOrElse("")
        val ranges = List(
            instance.extraction.source.getArgument1.getRange,
            instance.extraction.source.getRelation.getRange,
            instance.extraction.source.getArgument2.getRange)
        Content(sentence.asScala.toList, url, ranges map rangeToInterval)
      }.toList

      Group(title, list)
    }.filter(_.contents.size > 0).sortBy(-_.contents.size)
  }

  private final val pronouns: Set[String] = Set("he", "she", "they", "them",
     "that", "this", "who", "whom", "i", "you", "him", "her", "we",
     "it", "the", "a", "an")
  private final val nonQuestionableChars = Pattern.compile("[\\p{Lower}\\p{Digit} ]+")
  private final val stripExtraWS = Pattern.compile(" +")
  private final val stripChars= Pattern.compile("[^\\p{Graph} ]+")
  private final val leadingBadChars = Pattern.compile("^\\s*(\\\"|\\'|\\()\\s")
  private final val leadingArticle = Pattern.compile("^\\s*(the|this|these|those|that|a|an)\\s", Pattern.CASE_INSENSITIVE)
  private final val startCap = Pattern.compile(".*\\b[A-Z].*")
  private final val likelyError = Pattern.compile(".*(http|\\(|\\)|\\\"|\\[|thing).*", Pattern.CASE_INSENSITIVE)

  def filterInstances(inst: Instance[ReVerbExtraction]): Boolean = {
    def clean(arg: String) = {
      var clean = arg

      clean = stripChars.matcher(clean).replaceAll("");
      clean = stripExtraWS.matcher(clean).replaceAll(" ").trim();
      clean = leadingBadChars.matcher(clean).replaceAll("");
      clean = leadingArticle.matcher(clean).replaceAll("");

      clean.toLowerCase
    }

    val arg1clean = clean(inst.extraction.source.getArgument1.getTokensAsString)
    val arg2clean = clean(inst.extraction.source.getArgument1.getTokensAsString)
    val relclean = clean(inst.extraction.source.getRelation.getTokensAsString)

    if (arg1clean.length + relclean.length + arg2clean.length > 120) {
      false
    }
    else if (pronouns.contains(arg1clean) || pronouns.contains(arg2clean)) {
      false
    }
    else if (arg1clean.isEmpty || relclean.isEmpty || arg2clean.isEmpty) {
      false
    }
    else {
      true
    }
  }
}
