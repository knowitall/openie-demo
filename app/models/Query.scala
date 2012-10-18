package models

import java.io.{ObjectInputStream, FileInputStream, File}
import java.util.regex.Pattern
import scala.Option.option2Iterable
import edu.washington.cs.knowitall.browser.extraction._
import edu.washington.cs.knowitall.browser.lucene
import edu.washington.cs.knowitall.browser.lucene.{Timeout, Success, QuerySpec, LuceneFetcher, Limited}
import edu.washington.cs.knowitall.common.Resource.using
import edu.washington.cs.knowitall.common.Timing
import Query._
import akka.actor.{TypedProps, TypedActor}
import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api.Logger
import edu.washington.cs.knowitall.browser.extraction.InstanceDeduplicator
import edu.washington.cs.knowitall.browser.extraction.ExtractionRelation
import edu.washington.cs.knowitall.tool.postag.Postagger.prepositions
import controllers.routes

case class Query(
  arg1: Option[Query.Constraint],
  rel: Option[Query.Constraint],
  arg2: Option[Query.Constraint],
  corpora: Option[Query.CorporaConstraint]) {

  import Query._
  import edu.washington.cs.knowitall.tool.postag.PostaggedToken

  def arg1String = arg1.getOrElse("").toString
  def relString = rel.getOrElse("").toString
  def arg2String = arg2.getOrElse("").toString

  def toLowerCase: Query = {
    val lowerCasedQuery = Query(arg1.map(_.toLowerCase), rel.map(_.toLowerCase), arg2.map(_.toLowerCase), corpora)
    Logger.debug("Lowercasing query to %s from %s".format(lowerCasedQuery.toString, this.toString))
    lowerCasedQuery
  }
  
  override def toString = "(" + arg1String + ", " +
                                relString + ", " +
                                arg2String + 
                                corpora.map(c => ", " + c.toString).getOrElse("") + ")"

  def humanString = "a query with " + Iterable(
      arg1.map("Argument 1 containing '" + _ + "'"),
      rel.map("Relation containing '" + _ + "'"),
      arg2.map("Argument 2 containing '" + _ + "'")).flatten.mkString(" and ")

  def full = arg1.isDefined && rel.isDefined && arg2.isDefined

  def freeParts: List[ExtractionPart] = {
    Iterable((arg1, Argument1), (rel, Relation), (arg2, Argument2)).filter { case (constraint, part) =>
      constraint match {
        case None => true
        case Some(constraint) => constraint.free
      }
    }.map(_._2)(scala.collection.breakOut)
  }

  def fullParts: List[ExtractionPart] = {
    Iterable((arg1, Argument1), (rel, Relation), (arg2, Argument2)).filter { case (constraint, part) =>
      constraint match {
        case None => false
        case Some(constraint) => !constraint.free
      }
    }.map(_._2)(scala.collection.breakOut)
  }

  def filters: Seq[TypeFilter] = {
    var seq = Seq.empty[TypeFilter]

    def filtersFor(part: ExtractionPart, constraint: Constraint) = constraint match {
      case constraint: TypeConstraint => Some(PositiveStringTypeFilter(constraint.typ, List(part)))
      case _ => None
    }

    seq ++= this.arg1.flatMap(constraint => filtersFor(Argument1, constraint))
    seq ++= this.rel.flatMap(constraint => filtersFor(Relation, constraint))
    seq ++= this.arg2.flatMap(constraint => filtersFor(Argument2, constraint))

    seq
  }

  def execute(): Query.Result = {
    def group: REG => AnswerTitle = {
      def part(eg: REG, part: Symbol) = {
        part match {
          case 'rel => AnswerTitlePart(eg.rel.norm, Relation, eg.instances.iterator.map(_.extraction.relText).map(clean).toSeq, None, Set.empty)
          case 'arg1 => AnswerTitlePart(eg.arg1.norm, Argument1, eg.instances.iterator.map(_.extraction.arg1Text).map(clean).toSeq, eg.arg1.entity, eg.arg1.types.toSet)
          case 'arg2 => AnswerTitlePart(eg.arg2.norm, Argument2, eg.instances.iterator.map(_.extraction.arg2Text).map(clean).toSeq, eg.arg2.entity, eg.arg2.types.toSet)
        }
      }
      (this.arg1, this.rel, this.arg2) match {
        case (Some(_: Fixed), Some(_: Fixed), Some(_: Fixed)) => (eg: REG) =>
          AnswerTitle(" ", Seq(part(eg, 'arg1), part(eg, 'rel), part(eg, 'arg2)))

        case (Some(_: Fixed), Some(_: Fixed), _) => (eg: REG) => AnswerTitle("", Seq(part(eg, 'arg2)))
        case (_, Some(_: Fixed), Some(_: Fixed)) => (eg: REG) => AnswerTitle("", Seq(part(eg, 'arg1)))
        case (Some(_: Fixed), _, Some(_: Fixed)) => (eg: REG) => AnswerTitle("", Seq(part(eg, 'rel)))

        case (Some(_: Fixed), _, _) => (eg: REG) => AnswerTitle(" ", Seq(part(eg, 'rel), part(eg, 'arg2)))
        case (_, Some(_: Fixed), _) => (eg: REG) => AnswerTitle(", ", Seq(part(eg, 'arg1), part(eg, 'arg2)))
        case (_, _, Some(_: Fixed)) => (eg: REG) => AnswerTitle(", ", Seq(part(eg, 'arg1), part(eg, 'rel)))

        case _ => (eg: REG) => AnswerTitle(" ", Seq(part(eg, 'arg1), part(eg, 'rel), part(eg, 'arg2)))
      }
    }

    val (result, converted) = executeHelper()

    val (nsGroups, groups) = Timing.time { Answer.fromExtractionGroups(converted.toList, group, this.fullParts).filter(!_.title.text.trim.isEmpty) }

    Logger.debug("Converted to %d answers in %s".format(groups.size, Timing.Seconds.format(nsGroups)))

    result match {
      case lucene.Success(results) => Query.Success(groups)
      case lucene.Limited(results, hitCount) => Query.Limited(groups, hitCount)
      case lucene.Timeout(results, hitCount) => Query.Timeout(groups, hitCount)
    }
  }

  def executeRaw(): List[ExtractionGroup[ReVerbExtraction]] = executeHelper()._2.sortBy(-_.instances.size)

  private def executeHelper(): (lucene.ResultSet, List[ExtractionGroup[ReVerbExtraction]]) = {
    def query(constraint: Option[Query.Constraint]): Option[String] = constraint match {
      case Some(TermConstraint(term)) => Some(term)
      case _ => None
    }

    def queryTypes(constraint: Option[Query.Constraint]): Option[String] = constraint match {
      case Some(TypeConstraint(typ)) => Some(typ)
      case _ => None
    }

    def queryEntity(constraint: Option[Query.Constraint]): Option[String] = constraint match {
      case Some(EntityConstraint(entity)) => Some(entity)
      case _ => None
    }

    def queryCorpora(constraint: Option[Query.CorporaConstraint]): Option[String] = constraint match {
      case Some(CorporaConstraint(corpString)) => Some(corpString)
      case _ => None
    }

    // execute the query
    val spec = QuerySpec(query(this.arg1), query(this.rel), query(this.arg2), queryEntity(this.arg1), queryEntity(this.arg2), queryTypes(this.arg1), queryTypes(this.arg2), queryCorpora(this.corpora))
    val (nsQuery, result) = Timing.time {
      Query.fetcher.fetch(spec)
    }

    // open up the retrieved case class
    val (results, hitCount) = result match {
      case lucene.Success(results) => (results, results.size)
      case lucene.Limited(results, hitCount) => (results, hitCount)
      case lucene.Timeout(results, hitCount) => (results, hitCount)
    }
    Logger.debug(spec.toString + " searched with " + results.size + " groups (" + result.getClass.getSimpleName + ") in " + Timing.Seconds.format(nsQuery))

    val (nsRegroup, regrouped) = Timing.time {
      ReVerbExtractionGroup.indexGroupingToFrontendGrouping(results)
    }
    Logger.debug(spec.toString + " regrouped with " + regrouped.size + " answers (" + result.getClass.getSimpleName + ") in " + Timing.Seconds.format(nsRegroup))

    // apply backend deduplication
    // TODO: merge into index itself
    val (nsDeduped, deduped) = Timing.time {
      regrouped map InstanceDeduplicator.deduplicate
    }
    Logger.debug(spec.toString + " deduped with " + deduped.size + " answers (" + result.getClass.getSimpleName + ") in " + Timing.Seconds.format(nsDeduped))

    def entityFilter(entity: FreeBaseEntity) =
      entity.score > ENTITY_SCORE_THRESHOLD

    def typeFilter(typ: FreeBaseType) = {
      import TypeFilters._
      typ.valid
    }

    val (nsFiltered, filtered: List[ExtractionGroup[ReVerbExtraction]]) =
      Timing.time { deduped.iterator.map { reg =>
        // normalize fields and remove filtered entities/types
        val arg1Entity = reg.arg1.entity filter entityFilter
        val arg1EntityRemoved = reg.arg1.entity.isDefined && arg1Entity.isEmpty
        val arg1Types = if (!arg1EntityRemoved) reg.arg1.types filter typeFilter else Set.empty[FreeBaseType]

        val arg2Entity = reg.arg2.entity filter entityFilter
        val arg2EntityRemoved = reg.arg2.entity.isDefined && arg2Entity.isEmpty
        val arg2Types = if (!arg2EntityRemoved) reg.arg2.types filter typeFilter else Set.empty[FreeBaseType]

        reg.copy(
            instances = reg.instances filter filterCorpora filter filterInstances,
            arg1 = ExtractionArgument(clean(reg.arg1.norm), arg1Entity, arg1Types),
            rel  = reg.rel.copy(norm = clean(reg.rel.norm)),
            arg2 = ExtractionArgument(clean(reg.arg2.norm), arg2Entity, arg2Types)
          )
      } filter filterGroups(spec) filter filterRelation(spec) filter (_.instances.size > 0) toList
    }

    Logger.debug(spec.toString + " filtered with " + filtered.size + " answers in " + Timing.Seconds.format(nsFiltered))

    (result, filtered)
  }

  private val nonContentTag = "IN|TO|RB?".r

  private def filterCorpora(instance: Instance[_ <: Extraction]) = this.corpora match {
    case Some(CorporaConstraint(corporaString)) => corporaString.contains(instance.corpus)
    case _ => true
  }

  private def filterRelation(spec: QuerySpec)(group: ExtractionGroup[ReVerbExtraction]) = spec.relNorm match {
    // if the query does not constrain rel, we can ignore this filter
    case Some(queryRelNorm) => {
      val filteredRelNormTokens = queryRelNorm.toLowerCase.split(" ").filter { str => !prepositions.contains(str) } toSet;
      if (!filteredRelNormTokens.isEmpty) filterRelationHelper(filteredRelNormTokens, group) else true
    }
    case None => true
  }

  private def filterRelationHelper(filteredRelNormTokens: Set[String], group: ExtractionGroup[ReVerbExtraction]): Boolean = {
    group.instances.headOption match {
        // it's possible that the group is empty already due to some other filter.
        // If it is, ignore (a different filter checks for this)
        case Some(group) => {
          val extr = group.extraction
          def filterNonContent(tok: PostaggedToken): Boolean = nonContentTag.findFirstIn(tok.postag).isEmpty
          val groupRelNormTokens = extr.normTokens(extr.relInterval) filter filterNonContent
          val lastContentWord = groupRelNormTokens.last.string
          filteredRelNormTokens.contains(lastContentWord)
        }
        case None => true
      }
  }

  private def filterGroups(spec: QuerySpec)(group: ExtractionGroup[_ <: Extraction]): Boolean = {
    // if there are constraints,
    // apply them to each part
    def filterPart(constraint: Option[Constraint], entity: Option[FreeBaseEntity], types: Iterable[FreeBaseType]) = {
      constraint.map {
        _ match {
          case TypeConstraint(typ) => types.exists(_.typ equalsIgnoreCase typ)
          case EntityConstraint(ent) => entity.exists(_.name equalsIgnoreCase ent)
          case _ => true
        }
      }.getOrElse(true)
    }

    if (group.arg1.norm.trim.isEmpty || group.rel.norm.trim.isEmpty || group.arg2.norm.trim.isEmpty) {
      false
    } else {
      filterPart(this.arg1, group.arg1.entity, group.arg1.types) && filterPart(this.arg2, group.arg2.entity, group.arg2.types)
    }
  }

  /** Checks if the query matches specific conditions and returns a list of
   *  appropriate suggestion strings.
   *
   *  Cases:
   *    Only a single box is filled with more than three tokens
   *    All three boxes are filled and neither arg1/arg2 are type queries
   *    Arg1 starts with "who"
   *    Either arg1 or arg2 contain "what|which"
   *    All three boxes are filled and one of arg1/arg2 are type queries
   *
   *  @return a list of suggestion strings.
   */
  def specificSuggestions: List[String] = {
    import scala.collection.mutable.ListBuffer
    val lb = ListBuffer[String]()

    val a1 = arg1String.toLowerCase
    val r = relString.toString.toLowerCase
    val a2 = arg2String.toLowerCase

    val hasRel = r != ""

    // true if exactly one box b is filled and b has 3+ words
    val singleBoxFilled = (arg1.isDefined && !rel.isDefined && !arg2.isDefined && a1.split(" ").length >= 3) ||
                          (!arg1.isDefined && rel.isDefined && !arg2.isDefined && r.split(" ").length >= 3) ||
                          (!arg1.isDefined && !rel.isDefined && arg2.isDefined && a2.split(" ").length >= 3)

    // true if all boxes are filled and neither arg1 nor arg2 are type queries
    val filledAndNoTypes = full && !a1.startsWith("type:") && !a2.startsWith("type:")

    // true if at least one arg is an entity query
    val argIsEntity = a1.startsWith("entity:") || a2.startsWith("entity:")

    // true if arg1 starts with who (impossible for it to be exactly "who")
    val arg1StartsWho = a1.startsWith("who")

    // true if either a1 or a2 contains either "what" or "which"
    val argsContainW = a1.contains("what") || a1.contains("which") || a2.contains("what") || a2.contains("which") || a2.contains("who")

    // true if query is full and a1 or a2 are type queries
    val filledAndType = full && (a1.startsWith("type:") || a2.startsWith("type:"))

    if (argIsEntity)
      lb += "Entity queries usually return fewer but higher-precision results than non-entity queries. " +
      "Try removing \"entity:\" for more results."

    if (hasRel)
      lb += "Variations of the relation may yield better results. " +
      "Try making the relation more general or express it differently. " +
      "For example, instead of searching for actors who \"starred in\" Star Wars, you might try \"was in\" or \"did\"."

    if (arg1StartsWho)
      lb += "Instead of searching for \"who\", try \"type:person\" or leave it out altogether."

    if (singleBoxFilled)
      lb += "If you are putting an entire query in a single box, you will not get any results. " +
        "See the sample queries on the <a href=\"" + routes.Application.index() + "\">home page</a> for examples of well-formed queries."

    else if (filledAndNoTypes)
      lb += "Filling out all three boxes is seldom necessary. " +
          "Try replacing an argument with a type or leaving it out altogether. " +
          "For examples of this, click on the example queries on the <a href=\"" + routes.Application.index() + "\">home page</a>."

    else if (argsContainW)
      lb += "Consider searching for types, i.e.: \"type:Swimmer\" instead of \"which swimmer\""

    if (filledAndType)
      lb += "It is possible that a type you are searching for is not defined in our database. " +
          "Try making the type more general or removing it altogether."

    lb.toList
  }

  private def betterQuery(arg1: String, rel: String, arg2: String, corp: String): Query = {

    var newArg1 = arg1
    var newRel = rel
    var newArg2 = arg2

    val arg1Split = arg1.split(" ")
    val relSplit = rel.split(" ")
    val arg2Split = arg2.split(" ")

    // if (where, is, x) -> (x, is located in, _)
    if (arg1 == "where" && rel == "is" && arg2 != "") {
      return betterQuery(arg2, "is located in", "", corp)
    }

    // whether arg1 starts with who:
    val arg1Who = arg1.startsWith("who")

    // whether to make arg1/arg2 a typed parameter
    val makeArg1Typed = arg1Split.length == 2 && (arg1Split(0) == "which" || arg1Split(0) == "what")
    val makeArg2Typed = arg2Split.length == 2 && (arg2Split(0) == "which" || arg2Split(0) == "what")

    // if "who ..." -> type:person
    if (arg1Who) newArg1 = "type:person"

    // if "which/what x" -> "type:x"
    if (makeArg1Typed) newArg1 = "type:" + arg1Split(1)
    if (makeArg2Typed) newArg2 = "type:" + arg2Split(1)

    // removes periods, commas, bangs, and ?s from the end of strings
    def stripEndingPunct(str: String): String = {
      val badPattern = "[\\.,!?]+$".r
      str.replace(badPattern.findAllIn(str).mkString, "")
    }

    Query.fromStrings(stripEndingPunct(newArg1),
                      stripEndingPunct(newRel),
                      stripEndingPunct(newArg2),
                      corp)
  }

  /** Returns a string representation of a "better" query than this, if
   *  available.
   *
   *  Cases covered:
   *    arg1/arg2 are "which x" or "what x" -> "type:x"
   *    arg1 contains "who"
   *    any box ends with a punctuation mark -> remove punctuation
   *    query is of form (where, is, x) -> (x, is located in, _)
   *
   *  @return a "better" query than this. if no better query is available, returns
   *          a new query equivalent to this.
   *
   */
  def betterQuery: Query = {
    if (arg1String.toLowerCase == "where" && relString.toLowerCase == "is" && arg2.isDefined) {
      Query.fromStrings(arg2String, "is located in", "", corpora.getOrElse("").toString).betterQuery
    }
    betterQuery(
      arg1String.toLowerCase,
      relString.toLowerCase,
      arg2String.toLowerCase,
      corpora.getOrElse("").toString
    )
  }
}

object Query {
  type REG = ExtractionGroup[ReVerbExtraction]

  final val MAX_ANSWER_LENGTH = 60

  abstract class Result
  case class Success(groups: Seq[Answer]) extends Result
  case class Timeout(groups: Seq[Answer], hitCount: Int) extends Result
  case class Limited(groups: Seq[Answer], hitCount: Int) extends Result

  object Constraint {
    def parse(string: String) = {
      val lcase = string.toLowerCase.trim
      if (lcase.isEmpty) {
        None
      }
      else if (lcase == "who" || lcase == "who?" || lcase == "what" || lcase == "what?") {
        None
      }
      else if (lcase.startsWith("type:")) {
        Some(new TypeConstraint(string.drop(5).replaceAll(" ", "_")))
      }
      else if (lcase.startsWith("entity:")) {
        Some(new EntityConstraint(string.drop(7)))
      }
      else {
        Some(new TermConstraint(string))
      }
    }
  }

  /** Represents whether this part is free to be further constrainted
    * by type constraints with this constraint. */
  sealed abstract class Constraint {
    def free: Boolean = false
    def toLowerCase: Constraint
  }
  /** Represents whether this part of the answer will be shown in
    * the answer title.  For example, a term constraint will prevent
    * that part of the answer from being in the title. */
  trait Fixed
  case class TermConstraint(term: String) extends Constraint with Fixed {
    override def toString = term
    override def toLowerCase = TermConstraint(term.toLowerCase)
  }
  case class TypeConstraint(typ: String) extends Constraint {
    override def toString = "type:" + typ
    override def toLowerCase = TypeConstraint(typ.toLowerCase)
  }

  case class EntityConstraint(entity: String) extends Constraint with Fixed {
    override def toString = "entity:" + entity
    // Leaving toLowerCase as a no-op for now due to the problem's we've had w/ case sensitivity here.
    override def toLowerCase = this
  }

  case class CorporaConstraint(corpora: String) {
    override def toString = "corpora:" + corpora
  }

  val paths = Seq("/scratch/common/openie-demo/index-1.0.4",
    "/scratch2/common/openie-demo/index-1.0.4",
    "/scratch3/common/openie-demo/index-1.0.4",
    "/scratch4/common/openie-demo/index-1.0.4")

  val fetcher = TypedActor(Akka.system).typedActorOf(TypedProps[LuceneFetcher](), Akka.system.actorFor("akka://openie-lucene-server@reliable.cs.washington.edu:9002/user/fetcher"))

  /*
  val fetcher = new lucene.ParallelExtractionGroupFetcher(
      paths,
      /* max search groups (20k)  */
      20000,
      /* max read instances (10k) */
      10000,
      /* timout in millis (10s) */
      10000)
      */

  private final val CONFIDENCE_THRESHOLD: Double = 0.5
  private final val ENTITY_SCORE_THRESHOLD: Double = 5.0
  private final val pronouns: Set[String] = Set("he", "she", "they", "them",
   "that", "this", "who", "whom", "i", "you", "him", "her", "we",
   "it", "the", "a", "an")
  private final val nonQuestionableChars = Pattern.compile("[\\p{Lower}\\p{Digit} ]+")
  private final val stripExtraWS = Pattern.compile("\\s+")
  private final val stripChars= Pattern.compile("[^\\p{Graph}\\p{Cntrl} ]+")
  private final val leadingBadChars = Pattern.compile("^\\s*(\\.|,|\\\"|\\'|\\()\\s")
  private final val leadingArticle = Pattern.compile("^\\s*(the|this|these|those|that|a|an)\\s*", Pattern.CASE_INSENSITIVE)
  private final val startCap = Pattern.compile(".*\\b[A-Z].*")
  private final val likelyErrorPattern = Pattern.compile(".*(http|\\(|\\)|\\\"|\\[|thing).*", Pattern.CASE_INSENSITIVE)

  def apply(tuple: (Option[String], Option[String], Option[String], Option[String])): Query = tuple match {
    case (arg1, rel, arg2, corpora) =>
      new Query(arg1 flatMap Constraint.parse, rel flatMap Constraint.parse, arg2 flatMap Constraint.parse, corpora map CorporaConstraint.apply)
  }

  def noneIfEmpty(string: String): Option[String] =
    if (string.isEmpty) None
    else Some(string)

  def fromStrings(arg1: Option[String], rel: Option[String], arg2: Option[String], corpora: Option[String]): Query =
    this.apply(arg1 flatMap Constraint.parse, rel flatMap Constraint.parse, arg2 flatMap Constraint.parse, corpora map CorporaConstraint.apply)

  def fromStrings(arg1: String, rel: String, arg2: String, corpora: String): Query =
    this.fromStrings(noneIfEmpty(arg1), noneIfEmpty(rel), noneIfEmpty(arg2), noneIfEmpty(corpora))

  def fromFile(file: File) = {
    using (new FileInputStream(file)) { fis =>
      using (new ObjectInputStream(fis) {
              override def resolveClass(desc: java.io.ObjectStreamClass): Class[_] = {
                try { Class.forName(desc.getName, false, getClass.getClassLoader) }
                catch { case ex: ClassNotFoundException => super.resolveClass(desc) }
              }}) { in =>
        in.readObject().asInstanceOf[Iterable[ExtractionGroup[ReVerbExtraction]]]
      }
    }
  }

  def clean(string: String) = {
    var clean = string.trim

    clean = stripChars.matcher(clean).replaceAll("");
    clean = stripExtraWS.matcher(clean).replaceAll(" ").trim();
    clean = leadingBadChars.matcher(clean).replaceAll("");

    clean
  }

  private def filterInstances(inst: Instance[ReVerbExtraction]): Boolean = {
    def clean(arg: String) = {
      var clean = this.clean(arg.trim)

      clean = leadingArticle.matcher(clean).replaceAll("");

      clean.toLowerCase
    }

    def tooShort(part: String) = {
      part.size - nonQuestionableChars.matcher(part).replaceAll("").size <= 1
    }

    val relTokens = inst.extraction.sentenceTokens.slice(inst.extraction.relInterval.start, inst.extraction.relInterval.end)
    val arg2Tokens = inst.extraction.sentenceTokens.slice(inst.extraction.arg2Interval.start, inst.extraction.arg2Interval.end)

    val arg1clean = clean(inst.extraction.arg1Text)
    val arg2clean = clean(inst.extraction.arg2Text)
    val relclean = clean(inst.extraction.relText)
    val extr = arg1clean + relclean + arg2clean

    def negative = {
      val negatives = Set("no", "not", "none", "n't", "never")
      relTokens.exists(token =>
        negatives.contains(token.string.toLowerCase)
      ) ||
      arg2Tokens.exists(token =>
        negatives.contains(token.string.toLowerCase)
      )
    }

    def tooLong =
      inst.extraction.arg1Text.length + inst.extraction.arg2Text.length + inst.extraction.relText.length > MAX_ANSWER_LENGTH

    def containsPronoun =
      pronouns.contains(arg1clean) || pronouns.contains(arg2clean)

    def likelyError =
      likelyErrorPattern.matcher(arg1clean).matches() ||
      likelyErrorPattern.matcher(arg2clean).matches()

    if (negative ||
      tooLong ||
      containsPronoun ||
      inst.confidence < CONFIDENCE_THRESHOLD ||
      (arg1clean.isEmpty || relclean.isEmpty || arg2clean.isEmpty) ||
      (arg1clean == arg2clean) ||
      (nonQuestionableChars.matcher(extr).replaceAll("").size >= 5) ||
      (tooShort(arg1clean) || tooShort(relclean) || tooShort(arg2clean)) ||
      likelyError) {
      false
    }
    else {
      true
    }
  }

  /** @return a list of general suggestion strings for improving queries. */
  def generalSuggestions: List[String] = {
    import scala.collection.mutable.ListBuffer
    val lb = ListBuffer[String]()

    lb += "Make sure all words are spelled correctly."
    lb += "Click on the example queries on the home page for proper usage."
    lb += "Try making searches less specific."

    lb.toList
  }
}
