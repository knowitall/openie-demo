package controllers

import models.Query
import edu.knowitall.apps.QASystem
import edu.knowitall.apps.Components
import edu.knowitall.execution.IdentityExecutor
import edu.knowitall.parsing.QuestionParser
import edu.knowitall.execution.Tuple
import edu.knowitall.scoring.ScoredAnswerGroup
import edu.knowitall.triplestore.SolrClient
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

  private val solrUrl = "http://rv-n16.cs.washington.edu:10983/solr/triplestore"
  private val maxHits = 500
  private val solrClient = SolrClient(solrUrl, maxHits)
  private val solrServer = solrClient.server
  private val executor = IdentityExecutor(solrClient) // Need to be able to hang on to the solr server.
  private val grouper = Components.groupers("basic")
  private val scorer = Components.scorers("numDerivations")
  private val answerConverter = new AnswerConverter(solrServer)

  private def qaSystem(parser: QuestionParser) = QASystem(parser, executor, grouper, scorer)

  def fetch(query: Query): Executor.Result[Answer] = {
    val scoredAnswers = qaSystem(query.parser).answer(query.question)
    val answers = scoredAnswers map answerConverter.getAnswer
    Success(answers)
  }
}
