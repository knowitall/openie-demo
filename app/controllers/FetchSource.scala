package controllers

import java.io.ByteArrayInputStream
import java.io.ObjectInputStream
import scala.collection.mutable
import scala.Option.option2Iterable
import scala.collection.JavaConverters._
import scala.util.control.NonFatal
import org.apache.solr.client.solrj.SolrQuery
import org.apache.solr.client.solrj.impl.HttpSolrServer
import akka.actor.TypedActor
import akka.actor.TypedProps
import edu.knowitall.openie.models.ExtractionArgument
import edu.knowitall.openie.models.ExtractionGroup
import edu.knowitall.openie.models.ExtractionRelation
import edu.knowitall.openie.models.FreeBaseEntity
import edu.knowitall.openie.models.FreeBaseType
import edu.knowitall.openie.models.Instance
import edu.knowitall.openie.models.ReVerbExtraction
import edu.knowitall.common.Resource.using
import edu.knowitall.common.Timing
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.tokenize.OpenNlpTokenizer
import play.api.Play.current
import play.api.Logger
import play.libs.Akka
import controllers.Executor.{ Limited, Timeout, Success }
import com.twitter.bijection.Bijection
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.BlockingQueue
import java.util.concurrent.TimeUnit
import models.Query
import edu.knowitall.openie.models.Extraction
import edu.knowitall.openie.models.ExtractionCluster

abstract class FetchSource {
  // a fetcher returns a ResultSet contains extraction groups
  def fetch(query: Query): Executor.Result[ExtractionCluster[Extraction]]
}

object Fetch {
  val tokenizer = {
    Timing.timeThen {
      new OpenNlpTokenizer()
    } { ns => Logger.info("Initialized OpenNlpTokenizer (" + Timing.Seconds.format(ns) + ")") }
  }

  // turn a string into a query string.  We don't use a normalized field in SOLR
  // because extractions are grouped by lemmas--the actual text might be unique
  // between the instances--which one to use?
  def normalize(string: String) = {
    val tokenized = tokenizer.synchronized {
      Fetch.tokenizer.tokenize(string)
    }

    (tokenized.map(_.string) map MorphaStemmer.lemmatize).mkString(" ")
  }
}

/*
 * Probably should just delete this
 */

//case object SolrSource extends FetchSource {
//  private final val SOLRQUERY_MAX_ROWS = 500
//  import edu.knowitall.openie.models.serialize.Chill
//
//  // KRYO is not threadsafe, so make a queue of instances
//  val kryos = {
//    val capacity = 4
//    val q = new ArrayBlockingQueue[Bijection[AnyRef, Array[Byte]]](capacity)
//    for (i <- 1 to capacity) {
//      q.offer(Chill.createBijection())
//    }
//    q
//  }
//  def withElement[R](queue: BlockingQueue[Bijection[AnyRef, Array[Byte]]])(block: Bijection[AnyRef, Array[Byte]]=>R): R = {
//    var q: Bijection[AnyRef, Array[Byte]] = null
//    try {
//      q = queue.poll(60, TimeUnit.SECONDS)
//      block(q)
//    }
//    catch {
//      case NonFatal(e) =>
//        // recreate kryo instance
//        // the old one might be corrupt
//        q = Chill.createBijection()
//        throw e
//    }
//    finally {
//      if (q != null) {
//        queue.offer(q)
//      }
//    }
//  }
//  val solrUrl = current.configuration.getString("source.solr.url").getOrElse {
//    throw new Exception("Unspecified configuration 'source.solr.url'.")
//  }
//  val solr = new HttpSolrServer(solrUrl)
//
//  // SOLR settings
//  solr.setSoTimeout(30000); // socket read timeout
//  solr.setConnectionTimeout(30000);
//  solr.setDefaultMaxConnectionsPerHost(100);
//  solr.setMaxTotalConnections(100);
//  solr.setFollowRedirects(false); // defaults to false
//  solr.setAllowCompression(true);
//  solr.setMaxRetries(1); // defaults to 0.  > 1 not recommended.
//
//  def queryString(spec: TripleQuery) = {
//    def quote(string: String) = "\"" + string + "\""
//
//
//    def normalizeOr(string: String) = {
//      val tokenized = Fetch.tokenizer.synchronized {
//        Fetch.tokenizer.tokenize(string)
//      }
//
//      val normalized = (tokenized.map(_.string) map MorphaStemmer.lemmatize).mkString(" ")
//      if (normalized != string) "(" + quote(normalized) + " OR " + quote(string) + ")"
//      else quote(normalized)
//    }
//
//    // figure which fields we will query for
//    // (fieldName, fieldValue)
//    val parts = (Iterable(
//        "arg1", "rel", "arg2",
//        "arg1_types", "arg2_types",
//        "arg1_entity_name", "arg2_entity_name") zip Iterable(
//            spec.arg1StringField map normalizeOr, spec.relStringField map normalizeOr, spec.arg2StringField map normalizeOr,
//            spec.arg1TypeField map quote, spec.arg2TypeField map quote,
//            spec.arg1EntityField map quote, spec.arg2EntityField map quote)).flatMap {
//      case (a, b) => if (b.isEmpty) {
//        None
//      } else {
//        Some("+" + a, b.get)
//      }
//    }
//
//    // build the query text
//    parts.map { case (field, value) => field + ":" + value + "" }.mkString(" ")
//  }
//
//  override def fetch(query: TripleQuery) = {
//    val squery = new SolrQuery()
//
//    val queryText = queryString(query)
//
//    Logger.debug("SOLR query: " + queryText)
//
//    // set query options
//    squery.setQuery(queryText + " size:[50 TO *]")
//    squery.setSort("size", SolrQuery.ORDER.desc)
//    squery.setRows(SOLRQUERY_MAX_ROWS)
//    squery.setTimeAllowed(Executor.queryTimeout)
//
//    // submit query and await response
//    // TODO: make async
//    val (nsSolr, response) = Timing.time {
//      solr.query(squery).getResults()
//    }
//    Logger.debug("SOLR response received (" + Timing.Seconds.format(nsSolr) + ")  NumFound: " + response.getNumFound() + "  Size: " + response.size())
//
//
//    var instanceSize: Int = 0
//    val (ns, groups) =
//      Timing.time {
//        withElement(kryos) { kryo =>
//          // convert the results to ExtractionGroup[ReVerbExtraction]
//          for (result <- response.iterator.asScala if instanceSize < 20000) yield {
//            // deserialize instances
//            val bytes = result.getFieldValue("instances").asInstanceOf[Array[Byte]]
//            val (nsKryo, instances: List[Extraction]) = Timing.time {
//              kryo.invert(bytes)
//                .asInstanceOf[List[Extraction]]
//                // throw new IllegalArgumentException("Could not deserialize instances: " + bytes.toSeq.toString)
//            }
//            Logger.trace(s"Instances deserialized (${ Timing.Seconds.format(nsKryo) }) for (${ result.getFieldValue("text") }) from ${ bytes.size } bytes: ${ instances.size }")
//            instanceSize += instances.size
//
//            def buildArgument(argName: String) = {
//              ExtractionArgument(
//                norm = result.getFieldValue(argName).asInstanceOf[String],
//                entity = {
//                  // if there is an entity id, we have an entity
//                  if (!result.containsKey(argName + "_entity_id")) None
//                  else {
//                    val id = result.getFieldValue(argName + "_entity_id").asInstanceOf[String]
//                    val name = result.getFieldValue(argName + "_entity_name").asInstanceOf[String]
//                    val inlink_ratio = result.getFieldValue(argName + "_entity_inlink_ratio").asInstanceOf[Double]
//                    val score = result.getFieldValue(argName + "_entity_score").asInstanceOf[Double]
//                    Some(FreeBaseEntity(name, id, score, inlink_ratio))
//                  }
//                },
//                types =
//                  // if the set "fulltypes" is non-empty, we have types
//                  if (!result.containsKey(argName + "_fulltypes")) Set.empty
//                  else {
//                    val types = result.getFieldValue(argName + "_fulltypes").asInstanceOf[java.util.List[String]].asScala
//                    types.map(FreeBaseType.parse(_).get).toSet
//                  })
//            }
//
//            val rel = ExtractionRelation(result.getFieldValue("rel").asInstanceOf[String])
//            val arg1 = buildArgument("arg1")
//            val arg2 = buildArgument("arg2")
//
//            ExtractionCluster[Extraction](
//              arg1 = arg1,
//              rel = rel,
//              arg2 = arg2,
//              instances = instances)
//          }
//        }.toList
//      }
//
//    Logger.debug("groups created from SOLR response (" + Timing.Seconds.format(ns) + ")")
//    Logger.debug(s"$instanceSize instances from ${groups.size}/${response.size} groups")
//
//    // the result set is limited if SOLR limited the results or the
//    // instance limit limited the results
//    if (groups.size >= SOLRQUERY_MAX_ROWS || groups.size < response.size) {
//      Limited(groups)
//    }
//    else if (ns / Timing.Seconds.divisor > Executor.queryTimeout) {
//      Timeout(groups)
//    }
//    else {
//      Success(groups)
//    }
//  }
//}
