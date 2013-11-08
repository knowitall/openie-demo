package controllers

import models.Query
import edu.knowitall.apps.QASystem
import edu.knowitall.apps.Components
import edu.knowitall.execution.IdentityExecutor
import edu.knowitall.paraphrasing.Paraphraser
import edu.knowitall.parsing.QuestionParser
import edu.knowitall.execution.Tuple
import edu.knowitall.scoring.ScoredAnswerGroup
import edu.knowitall.triplestore.SolrClient
import edu.knowitall.execution.DefaultFilters
import edu.knowitall.execution.StopwordExecutor
import org.apache.solr.client.solrj.SolrServer
import org.apache.solr.client.solrj.SolrQuery
import org.apache.solr.common.SolrDocument
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.collection.immutable.Interval
import models.Answer
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

  def fetch(query: Query): Executor.Result[Answer] = {
    val scoredAnswers = qaSystem(query.paraphraser, query.parser).answer(query.question)
    val answers = answerConverter.getAnswers(scoredAnswers)
    Success(answers)
  }
}