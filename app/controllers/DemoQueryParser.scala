package controllers


import edu.knowitall.parsing.QuestionParser
import edu.knowitall.execution.ListConjunctiveQuery
import edu.knowitall.execution.TVariable
import edu.knowitall.execution.TConjunct
import edu.knowitall.execution.Search.{arg1, rel, arg2}
import edu.knowitall.execution.Search.TSField
import play.api.Logger
import models.TripleQuery

/**
 * 3-field demo-style (type:/entity:) parser
 * based on 8 field parser
 */
case class DemoTripleParser() extends QuestionParser {
  val baseParser = DemoQueryParser()
  val triplePat = """\(?(.+),(.+),(.+)\)?""".r
  override def parse(q: String) = q match {
    case triplePat(arg1, rel, arg2) => {
      val bigUglyQueryString = TripleQuery.fromStrings(arg1, rel, arg2, "").question
      baseParser.parse(bigUglyQueryString)
    }
  }
}

/**
 * 8-field parser (string, entity, type, rel, string, entity, type, corpus)
 */
case class DemoQueryParser() extends QuestionParser {

  private val arg1Entity = TSField("arg1_entity_name")
  private val arg1Types = TSField("arg1_types")
  private val arg2Entity = TSField("arg2_entity_name")
  private val arg2Types = TSField("arg2_types")
  private val corpora = TSField("corpora")

  private val fields = List(arg1, arg1Entity, arg1Types, rel, arg2, arg2Entity, arg2Types, corpora)

  private def tconjFromFields(
      name: String,
      a1s: String,
      a1e: String,
      a1t: String,
      r: String,
      a2s: String,
      a2e: String,
      a2t: String,
      corpus: String): TConjunct = {

    val raw = List(a1s, a1e, a1t, r, a2s, a2e, a2t, corpus)
    val lst = raw.map(_.trim)
    val attrs = lst.map(TConjunct.getTVal)
    val items = fields.zip(attrs).toMap
    TConjunct(name, items.toMap)
  }

  private val qpat = """\(?(.+),(.+),(.+),(.+),(.+),(.+),(.+),(.+)\)?""".r
  private def parseSingleConjunct(name: String, s: String): Option[TConjunct] = s match {
    case qpat(a1s, a1e, a1t, r, a2s, a2e, a2t, corpora) =>
      val conj = tconjFromFields(name, a1s, a1e, a1t, r, a2s, a2e, a2t, corpora)
      if (conj.values.values.forall(_.isInstanceOf[TVariable])) {
        Logger.info("Rejecting query with no literals")
        None
      } else {
        Some(conj)
      }
    case _ => {
      Logger.error("Couldn't parse: " + s)
      None
    }
  }

  private def parseAllConjuncts(s: String): Iterable[TConjunct] = {
    val parts = s.split(TConjunct.splitPat).toList.map(_.trim).filterNot(_ == "")
    val queries = { for ((s, i) <- parts.zipWithIndex;
                       q <- parseSingleConjunct(s"r$i", s)) yield q }.toList
    queries
  }

  private def parseListConjunctiveQuery(s: String): Option[ListConjunctiveQuery] = {
    val parts = s.split(":", 2)

    // if the answer variable is named
    if (parts.size == 2) {
      val left = parts(0)
      val qVars = TVariable.fromStringMult(parts(0)) match {
        case head :: rest => head :: rest
        case _ =>
          throw new IllegalArgumentException(s"Expected variable: $left")
      }
      val conjuncts = parseAllConjuncts(parts(1))
      Some(ListConjunctiveQuery(qVars, conjuncts.toList))

    // if the answer variable is NOT named
    } else if (parts.size == 1) {
      val s = parts(0)
      val conjuncts = parseAllConjuncts(s)
      val qVars = conjuncts.flatMap(_.vars).toList match {
        case v :: rest => v :: rest
        case _ => throw new IllegalArgumentException(s"Expected variable: $s")
      }
      Some(ListConjunctiveQuery(qVars.distinct, conjuncts.toList))
    } else {
      None
    }
  }

  override def parse(q: String) = parseListConjunctiveQuery(q) match {
    case Some(cq: ListConjunctiveQuery) => ListConjunctiveQuery.expandSetTLiterals(cq)
    case _ => List()
  }
}