package controllers

import models.Query
import edu.knowitall.apps.QASystem
import edu.knowitall.apps.Components
import edu.knowitall.execution.IdentityExecutor
import edu.knowitall.paraphrasing.Paraphraser
import edu.knowitall.paraphrasing.IdentityParaphraser
import edu.knowitall.parsing.QuestionParser
import edu.knowitall.execution.Tuple
import edu.knowitall.scoring.ScoredAnswerGroup
import edu.knowitall.triplestore.SolrClient
import edu.knowitall.execution.DefaultFilters
import edu.knowitall.execution.StopwordExecutor
import edu.knowitall.execution.StrSim
import org.apache.solr.client.solrj.SolrServer
import org.apache.solr.client.solrj.SolrQuery
import org.apache.solr.common.SolrDocument
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.collection.immutable.Interval
import models.Answer
import models.DerivationGroup
import Executor.Result
import Executor.Success
import Executor.Limited
import play.api.Logger

/**
 * A FetchSource backed by triplestore-qa that converts
 * ScoredAnswerGroups (returned by QASystem) to
 * ExtractionClusters.
 */
object TriplestoreSource extends FetchSource {

  private val solrUrl = "http://reliable.cs.washington.edu:10983/solr/triplestore"
  private val maxHits = 100
  private val solrClient = SolrClient(solrUrl, maxHits)
  private val solrServer = solrClient.server
  private val executor = DefaultFilters.wrap(StopwordExecutor(IdentityExecutor(solrClient))) // Need to be able to hang on to the solr server.
  private val grouper = Components.groupers("basic")
  private val scorer = Components.scorers("numDerivations")
  private val answerConverter = new AnswerConverter(solrServer)

  private def qaSystem(paraphraser: Paraphraser, parser: QuestionParser) = QASystem(paraphraser, parser, executor, grouper, scorer)

  def fetch(query: Query): Result[Answer] = {
    
    // special case "Who/What is" queries for regex parser...
    if (query.parserName == "regex") WhoIsDetector.detect(query.question) match {
      case Some(WhoIsDetected(parser)) => noFreebaseHack(fetch(query.question, IdentityParaphraser, parser))
      case None => fetch(query.question, query.paraphraser, query.parser)
    } else {
      fetch(query.question, query.paraphraser, query.parser)
    }
  }
  
  def fetch(question: String, pphraser: Paraphraser, parser: QuestionParser): Success[Answer] = {
    val scoredAnswers = qaSystem(pphraser, parser).answer(question)
    val answers = answerConverter.getAnswers(scoredAnswers)
    Success(answers)
  }
  
  def noFreebaseHack(s: Executor.Success[Answer]) = {
    val newAnswers = s.groups.map { answer => 
      val newdgroups = answer.dgroups.map { dg =>
        dg.copy(queryTriples = dg.queryTriples.map { case (q, ts) => 
          (q, ts.filter(_.source != "freebase")) 
        })  
      }
      answer.copy(dgroups = DerivationGroup.regroup(newdgroups).filter(_.resultsCount != 0))
    }
    s.copy(groups = newAnswers.filter(_.resultsCount != 0))
  }
}

case class WhoIsDetected(parser: QuestionParser)

object WhoIsDetector {
  
  import edu.knowitall.parsing.regex.RegexQuestionPattern
  import edu.knowitall.parsing.regex.RegexQuestionPatterns.whatWho
  import edu.knowitall.parsing.regex.RegexQuestionPatterns.rel
  import edu.knowitall.parsing.regex.RegexQuestionPatterns.ent
  import edu.knowitall.parsing.regex.RegexQuestionPatterns.punct
  import edu.knowitall.execution.Search.arg1
  import edu.knowitall.paraphrasing.ScoredParaphraseDerivation
  import edu.knowitall.paraphrasing.Paraphrase
  
  val pattern = new RegexQuestionPattern(
      Seq("ent"),
      s"$whatWho <lemma='be'> (<ent>:$ent) $punct?",
      Seq("($ent, $r, $x)", "($x, $r, $ent)"))
  
  
  def detect(question: String): Option[WhoIsDetected] = {
    
    val parses = pattern.parse(StrSim.lemmatize(question))
    if (parses.nonEmpty) {
      val ent = parses.head.conjuncts.head.values(arg1).toString
      val parser = new QuestionParser() { 
        def parse(q: String) = parses
      }
      Some(WhoIsDetected(parser))
    }
    else {
      None
    }
  }
}
