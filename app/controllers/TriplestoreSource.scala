package controllers

import models.Query
import edu.knowitall.openie.models.ExtractionCluster
import edu.knowitall.openie.models.ReVerbExtraction
import edu.knowitall.openie.models.Extraction
import edu.knowitall.apps.QASystem
import edu.knowitall.apps.Components
import edu.knowitall.execution.IdentityExecutor
import edu.knowitall.execution.Tuple
import edu.knowitall.scoring.ScoredAnswerGroup
import edu.knowitall.triplestore.SolrClient
import org.apache.solr.client.solrj.SolrServer
import org.apache.solr.client.solrj.SolrQuery
import org.apache.solr.common.SolrDocument
import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.openie.models.FreeBaseEntity
import edu.knowitall.openie.models.FreeBaseType
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
  private val parser = DemoQueryParser()
  private val executor = IdentityExecutor(solrClient) // Need to be able to hang on to the solr server.
  private val grouper = Components.groupers("basic")
  private val scorer = Components.scorers("numDerivations")
  private val converter = new ScoredAnswerConverter(solrServer)
  
  private val qaSystem = QASystem(parser, executor, grouper, scorer)
  
  /**
   * A temporary hack to turn a Query into input to FormalQuestionParser
   */
  private def transformQuery(query: Query): Option[String] = {

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
    
    if (fields.forall(_._2.isEmpty)) {
      Logger.info("Skipping query with no literals.")
      None
    }
    else {
      val fieldStrings = fields.map { case (tvar, field) => (field.getOrElse(tvar)) }
      val usedMandatoryVars = fieldStrings.filter(mandatoryVars.contains)
      Some(usedMandatoryVars.mkString(",") + " : " + fieldStrings.mkString(", "))
    }
  }

  def fetch(query: Query): Executor.Result[ExtractionCluster[Extraction]] = {
    val hackQueryString = transformQuery(query)
    hackQueryString match {
      case Some(qString) => {
        val scoredAnswers = qaSystem.answer(qString)
        val tuples = scoredAnswers.flatMap(_.derivations.map(_.etuple.tuple))
        val converted = tuples flatMap converter.convert
        Success(converted)
      }
      case None => Limited(Nil)
    }
  }
}

class ScoredAnswerConverter(val solrServer: SolrServer) {
  
  private val wsSplit = "\\s".r
  private val commaSplit = ",".r
  
  def convertDoc(doc: SolrDocument): ReVerbExtraction = {
    
    def getString(field: String) = doc.getFieldValue(field).asInstanceOf[String]
    def getInterval(field: String) = Interval.deserialize(getString(field))
    val arg1Interval = getInterval("arg1_interval")
    val relInterval  = getInterval("rel_interval")
    val arg2Interval = getInterval("arg2_interval")
    val urlString = getString("url")
    val tokens  = getString("sentence_text").split(" ")
    val postags = getString("postags").split(" ")
    val chunks  = getString("chunks").split(" ")
    val offsets = getString("term_offsets").split(" ").map(_.toInt)
    require(Seq(postags, chunks, offsets).forall(_.size == tokens.size), "Inconsistent number of elements between tokens/postags/chunks/offsets for id " + getString("id"))
    
    val chunkedTokens = tokens.zip(postags).zip(chunks).zip(offsets).map {
      case (((token, postag), chunk), offset) => new ChunkedToken(chunk, postag, token, offset)
    }
    
    ReVerbExtraction(chunkedTokens.toIndexedSeq, arg1Interval, relInterval, arg2Interval, urlString)
  }
  
  def convertSourceId(id: String): Option[ReVerbExtraction] = {
    
    // make the solr query
    val idQueryString = s"id:$id"
    val solrQuery = new SolrQuery(idQueryString)
    val response = solrServer.query(solrQuery)
    val docs = response.getResults()
    
    if (docs.size == 0) {
      Logger.error("Metadata doc not found for id: " + id)
      None
    } else {
      if (docs.size > 1) Logger.warn("Should only be one metadata doc per id, found %d docs for id %s".format(docs.size, id))
      Some(convertDoc(docs.get(0)))
    }
  }
  
  def convert(tuple: Tuple): Option[ExtractionCluster[Extraction]] = {
    
    def gs(s: String) = tuple.getString("r0."+s)
    
    val arg1 = gs("arg1").get
    val rel = gs("rel").get
    val arg2 = gs("arg2").get
    
    val arg1norm = Fetch.normalize(arg1)
    val relnorm = Fetch.normalize(rel)
    val arg2norm = Fetch.normalize(arg2)
    
    def getLink(arg: String): Option[FreeBaseEntity] = {
      gs(arg+"_entity_name") match {
        case Some(name) => {
          val id = gs(arg+"_entity_id").get
          val score = tuple.get("r0."+arg+"_entity_score").get.asInstanceOf[Double]
          val ratio = tuple.get("r0."+arg+"_entity_inlink_ratio").get.asInstanceOf[Double]
          Some(FreeBaseEntity(name, id, score, ratio))
        }
        case None => None
      }
    }
    
    def getTypes(arg: String): Set[FreeBaseType] = {
      val typeStrings = tuple.get("r0."+arg+"_fulltypes").toSeq.flatMap(any => any.asInstanceOf[List[String]])
      typeStrings.toSet.flatMap(FreeBaseType.parse)
    }
    
    val sourceIds = tuple.get("r0.source_ids").get.asInstanceOf[List[String]]
    val extractions = sourceIds flatMap convertSourceId
    
    if (extractions.isEmpty) {
      Logger.warn("Discarding empty ExtractionCluster, id: %s, tuple (%s, %s, %s)".format(gs("id"), arg1norm, relnorm, arg2norm))
      None
    } else Some(
      new ExtractionCluster(
        arg1norm,
        relnorm,
        arg2norm,
        getLink("arg1"),
        getLink("arg2"),
        getTypes("arg1"),
        getTypes("arg2"),
        extractions))
  }
}

