package controllers

import java.util.regex.Pattern
import edu.knowitall.openie.models.ExtractionArgument
import edu.knowitall.openie.models.ExtractionGroup
import edu.knowitall.openie.models.FreeBaseEntity
import edu.knowitall.openie.models.FreeBaseType
import edu.knowitall.openie.models.Instance
import edu.knowitall.openie.models.ReVerbExtraction
import edu.knowitall.openie.models.ReVerbExtractionGroup
import edu.knowitall.common.Timing
import edu.knowitall.tool.postag.PostaggedToken
import edu.knowitall.tool.postag.Postagger
import models.Answer
import models.AnswerPart
import models.TripleQuery
import models.TripleQuery.Constraint
import models.TripleQuery.CorporaConstraint
import models.TripleQuery.EntityConstraint
import models.TripleQuery.Fixed
import models.TripleQuery.TermConstraint
import models.TripleQuery.TypeConstraint
import models.TypeFilters.enrichFreeBaseType
import play.api.Logger
import edu.knowitall.openie.models.Extraction
import models.TypeFilters
import edu.knowitall.openie.models.util.ExtractionDeduplicator
import edu.knowitall.openie.models.ExtractionCluster

object Executor {
  type REG = ExtractionGroup[ReVerbExtraction]

  // parameters determining how deep to search
  val maxSearchGroups = 20000
  val maxReadInstances = 10000
  val queryTimeout = 10000

  // where data is coming from
  final val SOURCE: FetchSource = TriplestoreSource

  // minimum thresholds for extraction groups
  case class ExecutionSettings(
    val extractionConfidenceThreshold: Double = 0.5,
    val entityScoreThreshold: Double = 5.0,
    val maxAnswerLength: Int = 60
  )
  object ExecutionSettings {
    val default = ExecutionSettings()
  }

  // a representation of the result set
  abstract class Result[T]
  case class Success[T](groups: Seq[T]) extends Result[T]
  case class Timeout[T](groups: Seq[T]) extends Result[T]
  case class Limited[T](groups: Seq[T]) extends Result[T]

  def execute(query: TripleQuery, settings: ExecutionSettings = ExecutionSettings.default): Result[Answer] = {

    val (result, converted) = executeHelper(query, settings)

    result
  }

  def executeRaw(query: TripleQuery, settings: ExecutionSettings = ExecutionSettings.default): List[Answer] = executeHelper(query, settings)._2.sortBy(-_.contents.size)

  private def executeHelper(query: TripleQuery, settings: ExecutionSettings): (Result[Answer], List[Answer]) = {

    // execute the query
    val (nsQuery, result) = Timing.time {
      SOURCE.fetch(query)
    }

    // open up the retrieved case class
    val results = result match {
      case Success(results) => results
      case Limited(results) => results
      case Timeout(results) => results
    }

    (result, results.toList)
  }

  private val nonContentTag = "IN|TO|RB?".r
  private final val pronouns: Set[String] = Set("he", "she", "they", "them",
    "that", "this", "who", "whom", "i", "you", "him", "her", "we",
    "it", "the", "a", "an")
  private final val nonQuestionableChars = Pattern.compile("[\\p{Lower}\\p{Digit} ]+")
  private final val leadingArticle = Pattern.compile("^\\s*(the|this|these|those|that|a|an)\\s*", Pattern.CASE_INSENSITIVE)
  private final val startCap = Pattern.compile(".*\\b[A-Z].*")
  private final val likelyErrorPattern = Pattern.compile(".*(http|\\(|\\)|\\\"|\\[|thing).*", Pattern.CASE_INSENSITIVE)

  private final val daysOfWeek = Set("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday", "mon.", "tues.", "wed.", "thurs.", "fri.", "sat.", "sun.", "thurs", "tues", "mon", "last week")

}
