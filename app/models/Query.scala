package models

import java.io.{ObjectInputStream, FileInputStream, File}
import java.util.regex.Pattern
import scala.Option.option2Iterable
import edu.washington.cs.knowitall.browser.extraction.{ReVerbExtraction, FreeBaseType, FreeBaseEntity, Instance, ExtractionGroup, ReVerbExtractionGroup}
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

case class Query(
  arg1: Option[Query.Constraint],
  rel: Option[Query.Constraint],
  arg2: Option[Query.Constraint]) {

  import Query._
  import edu.washington.cs.knowitall.tool.postag.Postagger.prepositions
  import edu.washington.cs.knowitall.tool.postag.PostaggedToken

  def arg1String = arg1.getOrElse("")
  def relString = rel.getOrElse("")
  def arg2String = arg2.getOrElse("")

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
        case (Some(TermConstraint(arg1)), Some(TermConstraint(rel)), Some(TermConstraint(arg2))) => (eg: REG) =>
          AnswerTitle(" ", Seq(part(eg, 'arg1), part(eg, 'rel), part(eg, 'arg2)))

        case (Some(TermConstraint(arg1)), Some(TermConstraint(rel)), _) => (eg: REG) => AnswerTitle("", Seq(part(eg, 'arg2)))
        case (_, Some(TermConstraint(rel)), Some(TermConstraint(arg2))) => (eg: REG) => AnswerTitle("", Seq(part(eg, 'arg1)))
        case (Some(TermConstraint(arg1)), _, Some(TermConstraint(arg2))) => (eg: REG) => AnswerTitle("", Seq(part(eg, 'rel)))

        case (Some(TermConstraint(arg1)), _, _) => (eg: REG) => AnswerTitle(" ", Seq(part(eg, 'rel), part(eg, 'arg2)))
        case (_, Some(TermConstraint(rel)), _) => (eg: REG) => AnswerTitle(", ", Seq(part(eg, 'arg1), part(eg, 'arg2)))
        case (_, _, Some(TermConstraint(arg2))) => (eg: REG) => AnswerTitle(", ", Seq(part(eg, 'arg1), part(eg, 'rel)))

        case _ => (eg: REG) => AnswerTitle(" ", Seq(part(eg, 'arg1), part(eg, 'rel), part(eg, 'arg2)))
      }
    }

    val (result, converted) = executeHelper()

    val groups = Answer.fromExtractionGroups(converted.toList, group).filter(!_.title.text.trim.isEmpty)

    result match {
      case lucene.Success(results) => Query.Success(groups)
      case lucene.Limited(results, hitCount) => Query.Limited(groups, hitCount)
      case lucene.Timeout(results, hitCount) => Query.Timeout(groups, hitCount)
    }
  }

  def executeRaw(): List[ExtractionGroup[ReVerbExtraction]] = executeHelper()._2

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

    // execute the query
    val spec = QuerySpec(query(this.arg1), query(this.rel), query(this.arg2), queryEntity(this.arg1), queryEntity(this.arg2), queryTypes(this.arg1), queryTypes(this.arg2))
    val (nsQuery, result) = Timing.time {
      Query.fetcher.fetch(spec)
    }

    // open up the retrieved case class
    val (results, hitCount) = result match {
      case lucene.Success(results) => (results, results.size)
      case lucene.Limited(results, hitCount) => (results, hitCount)
      case lucene.Timeout(results, hitCount) => (results, hitCount)
    }
    Logger.debug(spec.toString + " searched with " + results.size + " results (" + result.getClass.getSimpleName + ") in " + Timing.Seconds.format(nsQuery))

    val (nsRegroup, regrouped) = Timing.time {
      ReVerbExtractionGroup.indexGroupingToFrontendGrouping(results)
    }
    Logger.debug(spec.toString + " regrouped with " + regrouped.size + " results (" + result.getClass.getSimpleName + ") in " + Timing.Seconds.format(nsRegroup))

    // apply backend deduplication
    // TODO: merge into index itself
    val (nsDeduped, deduped) = Timing.time {
      regrouped map InstanceDeduplicator.deduplicate
    }
    Logger.debug(spec.toString + " deduped with " + deduped.size + " results (" + result.getClass.getSimpleName + ") in " + Timing.Seconds.format(nsDeduped))

    def entityFilter(entity: FreeBaseEntity) =
      entity.score > ENTITY_SCORE_THRESHOLD

    def typeFilter(typ: FreeBaseType) = {
      import TypeFilters._
      typ.valid
    }

    val (nsFiltered, filtered: List[ExtractionGroup[ReVerbExtraction]]) =
      Timing.time { deduped.iterator.map { reg =>
        // normalize fields and remove filtered entities/types
        reg.copy(
            instances = reg.instances filter filterInstances,
            arg1 = reg.arg1.copy(
              norm = clean(reg.arg1.norm),
              entity = reg.arg1.entity filter entityFilter,
              types = reg.arg1.types filter typeFilter
            ),
            rel = reg.rel.copy(norm = clean(reg.rel.norm)), // A method that you can call here that will filter out rel according to stephen's criteria
            arg2 = reg.arg2.copy(
              norm = clean(reg.arg2.norm),
              entity = reg.arg2.entity filter entityFilter,
              types = reg.arg2.types filter typeFilter
            ))
      } filter filterGroups(spec) filter filterRelation(spec) filter (_.instances.size > 0) toList
    }

    Logger.debug(spec.toString + " filtered with " + filtered.size + " results in " + Timing.Seconds.format(nsFiltered))

    (result, filtered)
  }
  
  private def filterRelation(spec: QuerySpec)(group: ExtractionGroup[ReVerbExtraction]) = spec.relNorm match {
      // if the query does not constrain rel, we can ignore this filter 
      case Some(queryRelNorm) => {
        group.instances.headOption match { 
          // it's possible that the group is empty already due to some other filter.
          // If it is, ignore (a different filter checks for this)
          case Some(group) => {
            println("QueryRelNorm: %s".format(queryRelNorm))
            val extr = group.extraction
            println("Extr is: %s".format(extr))
            def filterNonContent(tok: PostaggedToken): Boolean = !prepositions.contains(tok.string.toLowerCase)
            val groupRelNormTokens = extr.normTokens(extr.relInterval) filter filterNonContent
            println("GroupRelNormTokens: %s".format(groupRelNormTokens))
            val lastContentWord = groupRelNormTokens.last.string
            println("lastContentWord: %s".format(lastContentWord))
            queryRelNorm.contains(lastContentWord)
          }
          case None => true
        }
      }
      case None => true
    }

  private def filterGroups(spec: QuerySpec)(group: ExtractionGroup[ReVerbExtraction]): Boolean = {
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
  sealed abstract class Constraint {
    def free: Boolean = false
  }
  case class TermConstraint(term: String) extends Constraint {
    override def toString = term
  }
  case class TypeConstraint(typ: String) extends Constraint {
    override def toString = "type:" + typ
  }
  case class EntityConstraint(entity: String) extends Constraint {
    override def toString = "entity:" + entity
  }

  val paths = Seq("/scratch/common/openie-demo/index-1.0.1",
    "/scratch2/common/openie-demo/index-1.0.1",
    "/scratch3/common/openie-demo/index-1.0.1",
    "/scratch4/common/openie-demo/index-1.0.1")

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

  def apply(tuple: (Option[String], Option[String], Option[String])): Query = tuple match {
    case (arg1, rel, arg2) =>
      new Query(arg1 flatMap Constraint.parse, rel flatMap Constraint.parse, arg2 flatMap Constraint.parse)
  }

  def noneIfEmpty(string: String): Option[String] =
    if (string.isEmpty) None
    else Some(string)

  def fromStrings(arg1: Option[String], rel: Option[String], arg2: Option[String]): Query =
    this.apply(arg1 flatMap Constraint.parse, rel flatMap Constraint.parse, arg2 flatMap Constraint.parse)

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
}
