package models

import edu.knowitall.parsing.regex.RegexQuestionParser
import edu.knowitall.execution.ConjunctiveQuery
import edu.knowitall.execution.TConjunct
import edu.knowitall.execution.TVal
import edu.knowitall.execution.TVariable
import edu.knowitall.execution.UnquotedTLiteral
import edu.knowitall.execution.QuotedTLiteral
import edu.knowitall.execution.SetTLiteral
import edu.knowitall.execution.Search.{arg1, rel, arg2}
import play.api.Logger

object QAQuery {

  private val regexParser = RegexQuestionParser()
  
  def fromQuestionForm(question: String, corpora: Option[String]): TripleQuery = {
    fromQuestion(question, corpora.getOrElse(""))
  }
  
  def fromQuestion(question: String, corpora: String): TripleQuery = {
    
    // try to parse the question
    val uqueries = regexParser.parse(question)
    // get just the conjunctivequeries...
    val conjunctiveQueries = uqueries.filter(_.isInstanceOf[ConjunctiveQuery]).map(_.asInstanceOf[ConjunctiveQuery])
    // get just the first one with a single conjunct
    conjunctiveQueries.find(_.conjuncts.size == 1).map(_.conjuncts.head) match {
      case Some(conjunct) => fromConjunct(conjunct, corpora)
      case None => TripleQuery.fromStrings("", "", "", corpora)
    }
    
  }
  
  private def fromConjunct(conjunct: TConjunct, corpora: String): TripleQuery = {
    
    def getLiteral(tval: TVal): String = tval match {
      case v: TVariable => ""
      case UnquotedTLiteral(v) => v
      case QuotedTLiteral(v) => v
      case SetTLiteral(vs) => {
        Logger.warn("Regex parser returned unsupported SetTLiteral: "+vs.mkString(" | "))
        ""
      }
      case _ => {
        Logger.warn("Regex parser returned unknown tval: " + tval.toString)
        ""
      }
    }
    
    val fields = Set(arg1, rel, arg2)
    val tvals = fields.map(f => (f, conjunct.values(f))).toMap // conjunct is required to have arg1, rel, arg2
    val literals = tvals.map { case (f, v) => (f, getLiteral(v)) }
    TripleQuery.fromStrings(literals(arg1), literals(rel), literals(arg2), corpora)
  }
}