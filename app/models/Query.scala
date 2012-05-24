package models

import edu.washington.cs.knowitall.browser.extraction.Instance
import edu.washington.cs.knowitall.common.Resource.using
import edu.washington.cs.knowitall.common.Timing
import edu.washington.cs.knowitall.browser.lucene.ParallelExtractionGroupFetcher
import edu.washington.cs.knowitall.browser.lucene.{QuerySpec, Success, Limited, Timeout}
import edu.washington.cs.knowitall.browser.extraction.ExtractionGroup
import edu.washington.cs.knowitall.browser.extraction.ReVerbExtraction
import java.util.regex.Pattern
import java.io.FileInputStream
import java.io.ObjectInputStream
import java.io.File
import play.api.Logger
import edu.washington.cs.knowitall.browser.extraction.{ FreeBaseType, FreeBaseEntity }
import akka.actor.TypedActor
import akka.actor.ActorSystem
import akka.actor.TypedProps
import edu.washington.cs.knowitall.browser.lucene.LuceneFetcher


case class Query(
  arg1: Option[Query.Constraint],
  rel: Option[Query.Constraint],
  arg2: Option[Query.Constraint]) {

  import Query._

  def arg1String = arg1.getOrElse("")
  def relString = rel.getOrElse("")
  def arg2String = arg2.getOrElse("")

  def humanString = "a query with " + Iterable(
      arg1.map("Argument 1 containing '" + _ + "'"),
      rel.map("Relation containing '" + _ + "'"),
      arg2.map("Argument 2 containing '" + _ + "'")).flatten.mkString(" and ")

  def execute() = {
    def part(eg: REG, part: Symbol) = part match {
      case 'rel => GroupTitlePart(eg.relNorm, eg.instances.iterator.map(_.extraction.relText).map(clean).toSeq, None, Set.empty)
      case 'arg1 => GroupTitlePart(eg.arg1Norm, eg.instances.iterator.map(_.extraction.arg1Text).map(clean).toSeq, eg.arg1Entity, eg.arg1Types.toSet)
      case 'arg2 => GroupTitlePart(eg.arg2Norm, eg.instances.iterator.map(_.extraction.arg2Text).map(clean).toSeq, eg.arg2Entity, eg.arg2Types.toSet)
    }

    def group: REG=>GroupTitle = (this.arg1, this.rel, this.arg2) match {
      case (Some(TermConstraint(arg1)), Some(TermConstraint(rel)), Some(TermConstraint(arg2))) => (eg: REG) => GroupTitle("", Seq(part(eg, 'arg2)))

      case (Some(TermConstraint(arg1)), Some(TermConstraint(rel)), _) => (eg: REG) => GroupTitle("", Seq(part(eg, 'arg2)))
      case (_, Some(TermConstraint(rel)), Some(TermConstraint(arg2))) => (eg: REG) => GroupTitle("", Seq(part(eg, 'arg1)))
      case (Some(TermConstraint(arg1)), _, Some(TermConstraint(arg2))) => (eg: REG) => GroupTitle("", Seq(part(eg, 'rel)))

      case (Some(TermConstraint(arg1)), _, _) => (eg: REG) => GroupTitle(" ", Seq(part(eg, 'rel), part(eg, 'arg2)))
      case (_, Some(TermConstraint(rel)), _) => (eg: REG) => GroupTitle(", ", Seq(part(eg, 'arg1), part(eg, 'arg2)))
      case (_, _, Some(TermConstraint(arg2))) => (eg: REG) => GroupTitle(", ", Seq(part(eg, 'arg1), part(eg, 'rel)))

      case _ => (eg: REG) => GroupTitle(" ", Seq(part(eg, 'arg1), part(eg, 'rel), part(eg, 'arg2)))
    }

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

    val spec = QuerySpec(query(this.arg1), query(this.rel), query(this.arg2), queryEntity(this.arg1), queryEntity(this.arg2), queryTypes(this.arg1), queryTypes(this.arg2))
    val (ns, (results, num)) = Timing.time {
      Query.fetcher.fetch(spec) match {
        case Success(results, instanceCount) => (results, instanceCount)
        case Limited(results, instanceCount, totalGroupCount) => (results, instanceCount)
        case Timeout(results, instanceCount, totalGroupCount) => (results, instanceCount)
      }
    }

    Logger.debug(spec.toString + " searched with " + num + " results in " + Timing.Seconds.format(ns))

    val filtered = results.filter { result =>
      def filterPart(constraint: Option[Constraint], entity: Option[FreeBaseEntity], types: Iterable[FreeBaseType]) = {
        constraint.map {
          _ match {
            case TypeConstraint(typ) => types.exists(_.typ equalsIgnoreCase typ)
            case EntityConstraint(ent) => entity.exists(_.name equalsIgnoreCase ent)
            case _ => true
          }
        }.getOrElse(true)
      }

      filterPart(this.arg1, result.arg1Entity, result.arg1Types) && filterPart(this.arg2, result.arg2Entity, result.arg2Types)
    }

    def entityFilter(entity: FreeBaseEntity) =
      entity.score > ENTITY_SCORE_THRESHOLD

    val converted = filtered.map { reg =>
      reg.copy(
          instances = reg.instances filter filterInstances,
          arg1Norm = clean(reg.arg1Norm),
          relNorm = clean(reg.relNorm),
          arg2Norm = clean(reg.arg2Norm),
          arg1Entity = reg.arg1Entity filter entityFilter,
          arg2Entity = reg.arg2Entity filter entityFilter)
    }.toList filter filterGroups filter (_.instances.size > 0)

    val groups = Group.fromExtractionGroups(converted.toList, group).filter(!_.title.text.trim.isEmpty)

    groups
  }
}

object Query {
  type REG = ExtractionGroup[ReVerbExtraction]

  final val MAX_ANSWER_LENGTH = 60

  object Constraint {
    def parse(string: String) = {
      if (string.toLowerCase.startsWith("type:")) {
        new TypeConstraint(string.drop(5).replaceAll(" ", "_"))
      }
      else if (string.toLowerCase.startsWith("entity:")) {
        new EntityConstraint(string.drop(7))
      }
      else {
        new TermConstraint(string)
      }
    }
  }
  sealed abstract class Constraint
  case class TermConstraint(term: String) extends Constraint {
    override def toString = term
  }
  case class TypeConstraint(typ: String) extends Constraint {
    override def toString = "type:" + typ
  }
  case class EntityConstraint(entity: String) extends Constraint {
    override def toString = "entity:" + entity
  }

  val paths = Seq("/scratch/common/openie-demo/test-index-0.0.5",
    "/scratch2/common/openie-demo/test-index-0.0.5",
    "/scratch3/common/openie-demo/test-index-0.0.5",
    "/scratch4/common/openie-demo/test-index-0.0.5")

  val system = ActorSystem("client")
  val fetcher = TypedActor(system).typedActorOf(TypedProps[LuceneFetcher](), system.actorFor("akka://openie-lucene-server@reliable.cs.washington.edu:9002/user/fetcher"))

  /*
  val fetcher = new ParallelExtractionGroupFetcher(
      paths,
      /* max search groups (20k) */
      20000,
      /* max read instances (10k) */
      10000,
      /* timout in millis (10s) */
      10000)
      */

  private final val CONFIDENCE_THRESHOLD: Double = 0.5
  private final val ENTITY_SCORE_THRESHOLD: Double = 2.0
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

  def apply(tuple: (Option[String], Option[String], Option[String])): Query = tuple match {
    case (arg1, rel, arg2) =>
      new Query(arg1.map(Constraint.parse), rel.map(Constraint.parse), arg2.map(Constraint.parse))
  }

  def noneIfEmpty(string: String): Option[String] =
    if (string.isEmpty) None
    else Some(string)

  def fromStrings(arg1: Option[String], rel: Option[String], arg2: Option[String]): Query =
    this.apply(arg1 map Constraint.parse, rel map Constraint.parse, arg2 map Constraint.parse)

  def fromStrings(arg1: String, rel: String, arg2: String): Query =
    this.fromStrings(noneIfEmpty(arg1), noneIfEmpty(rel), noneIfEmpty(arg2))

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
      part.size - nonQuestionableChars.matcher(part).replaceAll("").size <= 2
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

  private def filterGroups(group: ExtractionGroup[ReVerbExtraction]): Boolean = {
    if (group.arg1Norm.trim.isEmpty || group.relNorm.trim.isEmpty || group.arg2Norm.trim.isEmpty) {
      false
    }
    else {
      true
    }
  }

  private def mapGroup(group: ExtractionGroup[ReVerbExtraction]): ExtractionGroup[ReVerbExtraction] = {
    def entityFilter(entity: FreeBaseEntity) =
      entity.score > ENTITY_SCORE_THRESHOLD

    new ExtractionGroup[ReVerbExtraction](
        group.arg1Norm,
        group.relNorm,
        group.arg2Norm,
        group.arg1Entity filter entityFilter,
        group.arg2Entity filter entityFilter,
        group.arg1Types,
        group.arg2Types,
        group.instances)
  }
}
