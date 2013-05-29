package controllers

import java.io.ByteArrayInputStream
import java.io.ObjectInputStream

import scala.Option.option2Iterable
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.JavaConverters.asScalaIteratorConverter

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
import edu.knowitall.browser.lucene
import edu.knowitall.browser.lucene.QuerySpec
import edu.knowitall.common.Resource.using
import edu.knowitall.common.Timing
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.tokenize.OpenNlpTokenizer
import play.api.Play.current
import play.api.Logger
import play.libs.Akka

sealed abstract class FetchSource {
  // a fetcher returns a ResultSet contains extraction groups
  def fetch(querySpec: QuerySpec): lucene.ResultSet
}

object Fetch {
  val tokenizer = {
    Timing.timeThen {
      new OpenNlpTokenizer()
    } { ns => Logger.info("Initialized OpenNlpTokenizer (" + Timing.Seconds.format(ns) + ")") }
  }
}

case object SolrSource extends FetchSource {
  import edu.knowitall.openie.models.serialize.Chill

  val kryo = Chill.createInjection()
  val solr = new HttpSolrServer(current.configuration.getString("source.solr.url").get)

  // SOLR settings
  solr.setSoTimeout(20000); // socket read timeout
  solr.setConnectionTimeout(20000);
  solr.setDefaultMaxConnectionsPerHost(100);
  solr.setMaxTotalConnections(100);
  solr.setFollowRedirects(false); // defaults to false
  solr.setAllowCompression(true);
  solr.setMaxRetries(1); // defaults to 0.  > 1 not recommended.

  override def fetch(spec: QuerySpec) = {
    val squery = new SolrQuery()

    // turn a string into a query string.  We don't use a normalized field in SOLR
    // because extractions are grouped by lemmas--the actual text might be unique
    // between the instances--which one to use?
    def normalize(string: String) = {
      val tokenized = Fetch.tokenizer.synchronized {
        Fetch.tokenizer.tokenize(string)
      }

      (tokenized.map(_.string) map MorphaStemmer.lemmatize).mkString(" ")
    }

    // figure which fields we will query for
    // (fieldName, fieldValue)
    val parts = (Iterable(
        "arg1", "rel", "arg2",
        "arg1_types", "arg2_types",
        "arg1_entity", "arg2_entity") zip Iterable(
            spec.arg1 map normalize, spec.rel map normalize, spec.arg2 map normalize,
            spec.arg1Types, spec.arg2Types,
            spec.arg1Entity, spec.arg2Entity)).flatMap {
      case (a, b) => if (b.isEmpty) {
        None
      } else {
        Some("+" + a, b.get)
      }
    }

    // build the query text
    val queryText = parts.map { case (field, value) => field + ":\"" + value + "\"" }.mkString(" ")

    Logger.debug("SOLR query: " + queryText)

    // set query options
    squery.setQuery(queryText + " size:[50 TO *]")
    squery.setSort("size", SolrQuery.ORDER.desc)
    squery.setRows(1000)
    squery.setTimeAllowed(Executor.queryTimeout)

    // submit query and await response
    // TODO: make async
    val response = try {
      Timing.timeThen {
        solr.query(squery)
      } { ns =>
        Logger.debug("solr response received (" + Timing.Seconds.format(ns) + ")")
      }
    }

    import scala.collection.JavaConverters._
    val groups =
      Timing.timeThen {
        // convert the results to ExtractionGroup[ReVerbExtraction]
        for (result <- response.getResults().asScala) yield {
          // deserialize instances
          val bytes = result.getFieldValue("instances").asInstanceOf[Array[Byte]]
          val instances: List[Instance[ReVerbExtraction]] =
            kryo.invert(bytes)
              .getOrElse(throw new IllegalArgumentException("Could not deserialize instances: " + bytes.toSeq.toString))
              .asInstanceOf[List[Instance[ReVerbExtraction]]]

          def buildArgument(argName: String) = {
            ExtractionArgument(
              norm = result.getFieldValue(argName).asInstanceOf[String],
              entity = {
                // if there is an entity id, we have an entity
                if (!result.containsKey(argName + "_entity_id")) None
                else {
                  val id = result.getFieldValue(argName + "_entity_id").asInstanceOf[String]
                  val name = result.getFieldValue(argName + "_entity_name").asInstanceOf[String]
                  val inlink_ratio = result.getFieldValue(argName + "_entity_inlink_ratio").asInstanceOf[Double]
                  val score = result.getFieldValue(argName + "_entity_score").asInstanceOf[Double]
                  Some(FreeBaseEntity(name, id, score, inlink_ratio))
                }
              },
              types =
                // if the set "fulltypes" is non-empty, we have types
                if (!result.containsKey(argName + "_fulltypes")) Set.empty
                else {
                  val types = result.getFieldValue(argName + "_fulltypes").asInstanceOf[java.util.List[String]].asScala
                  types.map(FreeBaseType.parse(_).get).toSet
                })
          }

          val rel = ExtractionRelation(result.getFieldValue("rel").asInstanceOf[String])
          val arg1 = buildArgument("arg1")
          val arg2 = buildArgument("arg2")

          ExtractionGroup[ReVerbExtraction](
            arg1 = arg1,
            rel = rel,
            arg2 = arg2,
            instances = instances.toSet)
        }
      }
    { ns =>
      Logger.debug("groups created from SOLR response (" + Timing.Seconds.format(ns) + ")")
    }

    Logger.debug("groups retrieved: " + groups.size)
    lucene.Success(groups.toList)
  }
}
