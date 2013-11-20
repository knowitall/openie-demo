package controllers

import models.Query
import org.slf4j.LoggerFactory
import edu.knowitall.apps.QASystem
import edu.knowitall.apps.Components
import edu.knowitall.execution.IdentityExecutor
import edu.knowitall.paraphrasing.Paraphraser
import edu.knowitall.paraphrasing.IdentityParaphraser
import edu.knowitall.parsing.QuestionParser
import edu.knowitall.execution.Tuple
import edu.knowitall.scoring.ScoredAnswerGroup
import edu.knowitall.triplestore._
import edu.knowitall.execution.Search.TSQuery
import edu.knowitall.execution.DefaultFilters
import edu.knowitall.execution.StopwordExecutor
import edu.knowitall.execution.StrSim
import org.apache.solr.client.solrj.impl.HttpSolrServer
import org.apache.solr.client.solrj.SolrServer
import org.apache.solr.client.solrj.SolrQuery
import org.apache.solr.common.SolrDocument
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.collection.immutable.Interval
import scala.collection.JavaConversions._
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

  private val solrUrl = "http://rv-n16.cs.washington.edu:10983/solr/triplestore"
  private val maxHits = 100
  private val baseSolrClient = DemoSolrClient(solrUrl, maxHits)
  private val baseSolrServer = baseSolrClient.server
  private val cachedClient = CachedTriplestoreClient(baseSolrClient)
  private val executor = DefaultFilters.wrap(StopwordExecutor(IdentityExecutor(cachedClient))) // Need to be able to hang on to the solr server.
  private val grouper = Components.groupers("basic")
  private val scorer = Components.scorers("numDerivations")
  private val answerConverter = new AnswerConverter(baseSolrServer)

  private def qaSystem(paraphraser: Paraphraser, parser: QuestionParser) = QASystem(paraphraser, parser, executor, grouper, scorer)

  def fetch(query: Query): Result[Answer] = {
    
    // special case "Who/What is" queries for regex parser...
    if (query.parserName == "regex") WhoIsDetector.detect(query.question) match {
      case Some(WhoIsDetected(parser)) => fetch(query.question, IdentityParaphraser, parser)
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

/**
 * Same as SolrClient (in triplestore-qa) except that it queries for
 * results in descending order of 'size' (the number of triples).
 * 
 * TODO: Make SolrClient a class that we can inherit from, rather than
 * copying all of the code (it's a case class and case object now).
 * 
 * Especially inherit and override buildSolrQuery, which is in the case object.  
*/
case class DemoSolrClient(url: String, hits: Int = 10) extends TriplestoreClient {

  def this() = this(SolrClient.defaultUrl, SolrClient.defaultMaxHits)
  
  val logger = LoggerFactory.getLogger(this.getClass)
  
  val server = new HttpSolrServer(url)
  
  val defaultMaxHits = hits

  /**
* Returns the number of documents in Solr that match the given query.
*/
  def count(q: TSQuery): Long = {
    val query = SolrClient.buildCountQuery(q)
    val resp = server.query(query)
    val c = resp.getResults().getNumFound()
    logger.info(s"Found $c hits for query: ${q.toQueryString}")
    return c
  }
  
  /**
* Searches Solr and returns Tuple objects.
*/
  def search(q: TSQuery, maxHits: Int = defaultMaxHits): List[Tuple] ={
    logger.info(s"Searching for query: ${q.toQueryString}")
    val query = DemoSolrClient.buildQuery(q)
    query.setRows(hits)
    val resp = server.query(query)
    // Careful: SolrDocument (e.g. resp.getResults()) is a bad implementation of a java collection,
    // throwing IllegalOperationException on required methods (like iterator()).
    val results = resp.getResults().toList.map(SolrClient.docToTuple)
    val n = results.size
    logger.info(s"Loaded $n tuples into memory for query: ${q.toQueryString}")
    return results
  }
}

case object DemoSolrClient {
  
  def buildQuery(q: TSQuery): SolrQuery =
    new SolrQuery(q.toQueryString).setSort("size", SolrQuery.ORDER.desc)
}
