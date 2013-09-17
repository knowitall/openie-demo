package controllers

import java.util.regex.Pattern
import models.Answer
import models.Query

object Executor {

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

  def execute(query: Query, settings: ExecutionSettings = ExecutionSettings.default): Result[Answer] = {

    SOURCE.fetch(query)
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
