package models

import java.io.{ ObjectInputStream, FileInputStream, File }
import java.util.regex.Pattern
import scala.Option.option2Iterable
import edu.knowitall.openie.models.{ExtractionGroup, ReVerbExtraction}
import edu.knowitall.common.Resource.using
import edu.knowitall.common.Timing
import akka.actor.{ TypedProps, TypedActor }
import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api.Logger
import controllers.routes

case class TripleQuery(
  arg1: Option[TripleQuery.Constraint],
  rel: Option[TripleQuery.Constraint],
  arg2: Option[TripleQuery.Constraint],
  corpora: Option[TripleQuery.CorporaConstraint]) extends Query {

  import TripleQuery._
  import edu.knowitall.tool.postag.PostaggedToken

  override val question = toDemoQueryString(this)
  override val parserName = "demo-triple"
  
  def arg1String = arg1.getOrElse("").toString
  def relString = rel.getOrElse("").toString
  def arg2String = arg2.getOrElse("").toString

  def arg1StringField: Option[String] = arg1 match {
    case Some(TermConstraint(term)) => Some(term)
    case _ => None
  }
  def relStringField: Option[String] = rel match {
    case Some(TermConstraint(term)) => Some(term)
    case _ => None
  }
  def arg2StringField: Option[String] = arg2 match {
    case Some(TermConstraint(term)) => Some(term)
    case _ => None
  }
  def arg1TypeField: Option[String] = arg1 match {
    case Some(TypeConstraint(typ)) => Some(typ)
    case _ => None
  }
  def arg2TypeField: Option[String] = arg2 match {
    case Some(TypeConstraint(typ)) => Some(typ)
    case _ => None
  }
  def arg1EntityField: Option[String] = arg1 match {
    case Some(EntityConstraint(entity)) => Some(entity)
    case _ => None
  }
  def arg2EntityField: Option[String] = arg2 match {
    case Some(EntityConstraint(entity)) => Some(entity)
    case _ => None
  }
  def corpusField: Option[String] = corpora match {
    case Some(CorporaConstraint(corpString)) => Some(corpString)
    case _ => None
  }

  override def toString = "(" + arg1String + ", " +
    relString + ", " +
    arg2String +
    corpora.map(c => ", " + c.toString).getOrElse("") + ")"

  def full = arg1.isDefined && rel.isDefined && arg2.isDefined

//  def freeParts: Set[String] = {
//    Iterable((arg1, "r0.arg1"), (rel, "r0.rel"), (arg2, "r0.arg2")).filter {
//      case (constraint, part) =>
//        constraint match {
//          case None => true
//          case Some(constraint) => constraint.free
//        }
//    }.map(_._2).toSet
//  }

//  def filters: Seq[TypeFilter] = {
//    var seq = Seq.empty[TypeFilter]
//
//    def filtersFor(part: String, constraint: Constraint) = constraint match {
//      case constraint: TypeConstraint => Some(PositiveStringTypeFilter(constraint.typ, Set(part)))
//      case _ => None
//    }
//
//    seq ++= this.arg1.flatMap(constraint => filtersFor("r0.arg1", constraint))
//    seq ++= this.rel.flatMap(constraint => filtersFor("r0.rel", constraint))
//    seq ++= this.arg2.flatMap(constraint => filtersFor("r0.arg2", constraint))
//
//    seq
//  }

  /**
   * Checks if the query matches specific conditions and returns a list of
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

  /**
   * Returns a string representation of a "better" query than this, if
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
  def betterQuery: TripleQuery = {
    if (arg1String.toLowerCase == "where" && relString.toLowerCase == "is" && arg2.isDefined) {
      TripleQuery.fromStrings(arg2String, "is located in", "", corpora.getOrElse("").toString).betterQuery
    }
    TripleQuery.betterQuery(
      arg1String.toLowerCase,
      relString.toLowerCase,
      arg2String.toLowerCase,
      corpora.getOrElse("").toString)
  }
}

object TripleQuery {
  type REG = ExtractionGroup[ReVerbExtraction]
  
  val parser = controllers.DemoQueryParser()
  
    /**
   * A hack to turn a Query into input to FormalQuestionParser
   */
  def toDemoQueryString(query: TripleQuery): String = {

    def quote(s: String) = "\"%s\"" format s
    
    val fields = List(
      ("$a1s", query.arg1StringField),
      ("$a1e", query.arg1EntityField),
      ("$a1t", query.arg1TypeField),
      ("$rel", query.relStringField),
      ("$a2s", query.arg2StringField),
      ("$a2e", query.arg2EntityField),
      ("$a2t", query.arg2TypeField),
      ("$corp",query.corpusField))
    
    val mandatoryVars = Set("$a1s", "$rel", "$a2s")
    
    val fieldStrings = fields.map { case (tvar, field) => (field.getOrElse(tvar)) }
    val usedMandatoryVars = fieldStrings.filter(mandatoryVars.contains)
    usedMandatoryVars.mkString(",") + " : " + fieldStrings.mkString(", ")
  }
  
  
  object Constraint {
    def parse(string: String) = {
      val lcase = string.toLowerCase.trim
      if (lcase.isEmpty) {
        None
      } else if (lcase == "who" || lcase == "who?" || lcase == "what" || lcase == "what?") {
        None
      } else if (lcase.startsWith("type:")) {
        Some(new TypeConstraint(string.drop(5).replaceAll(" ", "_")))
      } else if (lcase.startsWith("entity:")) {
        Some(new EntityConstraint(string.drop(7)))
      } else {
        Some(new TermConstraint(string))
      }
    }
  }

  /**
   * Represents whether this part is free to be further constrainted
   * by type constraints with this constraint.
   */
  sealed abstract class Constraint {
    def free: Boolean = false
    def toLowerCase: Constraint
  }
  /**
   * Represents whether this part of the answer will be shown in
   * the answer title.  For example, a term constraint will prevent
   * that part of the answer from being in the title.
   */
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

  def apply(tuple: (Option[String], Option[String], Option[String], Option[String])): TripleQuery = tuple match {
    case (arg1, rel, arg2, corpora) =>
      new TripleQuery(arg1 flatMap Constraint.parse, rel flatMap Constraint.parse, arg2 flatMap Constraint.parse, corpora map CorporaConstraint.apply)
  }

  def noneIfEmpty(string: String): Option[String] =
    if (string.isEmpty) None
    else Some(string)

  def fromStrings(arg1: Option[String], rel: Option[String], arg2: Option[String], corpora: Option[String]): TripleQuery =
    this.apply(arg1 flatMap Constraint.parse, rel flatMap Constraint.parse, arg2 flatMap Constraint.parse, corpora map CorporaConstraint.apply)

  def fromStrings(arg1: String, rel: String, arg2: String, corpora: String): TripleQuery =
    this.fromStrings(noneIfEmpty(arg1), noneIfEmpty(rel), noneIfEmpty(arg2), noneIfEmpty(corpora))

  def fromFile(file: File) = {
    using(new FileInputStream(file)) { fis =>
      using(new ObjectInputStream(fis) {
        override def resolveClass(desc: java.io.ObjectStreamClass): Class[_] = {
          try { Class.forName(desc.getName, false, getClass.getClassLoader) }
          catch { case ex: ClassNotFoundException => super.resolveClass(desc) }
        }
      }) { in =>
        in.readObject().asInstanceOf[Iterable[ExtractionGroup[ReVerbExtraction]]]
      }
    }
  }

  private final val stripExtraWS = Pattern.compile("\\s+")
  private final val stripChars = Pattern.compile("[^\\p{Graph}\\p{Cntrl} ]+")
  private final val leadingBadChars = Pattern.compile("^\\s*(\\.|,|\\\"|\\'|\\()\\s")

  def clean(string: String) = {
    var clean = string.trim

    clean = stripChars.matcher(clean).replaceAll("");
    clean = stripExtraWS.matcher(clean).replaceAll(" ").trim();
    clean = leadingBadChars.matcher(clean).replaceAll("");

    clean
  }

  /** @return a list of general suggestion strings for improving queries. */
  def generalSuggestions: List[String] = {
    import scala.collection.mutable.ListBuffer
    val lb = ListBuffer[String]()

    lb += "Make sure all words are spelled correcquerytly."
    lb += "Click on the example queries on the home page for proper usage."
    lb += "Try making searches less specific."

    lb.toList
  }

  private def betterQuery(arg1: String, rel: String, arg2: String, corp: String): TripleQuery = {

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

    TripleQuery.fromStrings(stripEndingPunct(newArg1),
      stripEndingPunct(newRel),
      stripEndingPunct(newArg2),
      corp)
  }
}
