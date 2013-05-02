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

case object LuceneSource extends FetchSource {
  val paths = Seq("/scratch/common/openie-demo/index-1.0.4",
    "/scratch2/common/openie-demo/index-1.0.4",
    "/scratch3/common/openie-demo/index-1.0.4",
    "/scratch4/common/openie-demo/index-1.0.4")

  lazy val fetcher = new lucene.ParallelExtractionGroupFetcher(
    paths,
    /* max search groups (20k)  */
    Executor.maxSearchGroups,
    /* max read instances (10k) */
    Executor.maxReadInstances,
    /* timout in millis (10s) */
    Executor.queryTimeout)

  override def fetch(querySpec: QuerySpec) = fetcher.getGroups(querySpec)
}

/*
case object ActorSource extends FetchSource {
  lazy val fetcher = TypedActor(Akka.system).typedActorOf(TypedProps[LuceneFetcher](), Akka.system.actorFor("akka://openie-lucene-server@reliable.cs.washington.edu:9002/user/fetcher"))
  override def fetch(querySpec: QuerySpec) = fetcher.fetch(querySpec)
}
*/

case object SolrSource extends FetchSource {
  import edu.knowitall.openie.models.serialize.Chill
  val kryo = Chill.createInjection()
  val solr = new HttpSolrServer("http://rv-n16.cs.washington.edu:8983/solr/")
  solr.setSoTimeout(20000); // socket read timeout
  solr.setConnectionTimeout(20000);
  solr.setDefaultMaxConnectionsPerHost(100);
  solr.setMaxTotalConnections(100);
  solr.setFollowRedirects(false); // defaults to false
  solr.setAllowCompression(true);
  solr.setMaxRetries(1); // defaults to 0.  > 1 not recommended.

  override def fetch(spec: QuerySpec) = {
    val squery = new SolrQuery()

    def normalize(string: String) = {
      val tokenized = Fetch.tokenizer.synchronized {
        Fetch.tokenizer.tokenize(string)
      }

      (tokenized.map(_.string) map MorphaStemmer.lemmatize).mkString(" ")
    }

    val parts = (Iterable("arg1", "rel", "arg2", "arg1_types", "arg2_types", "arg1_entity", "arg2_entity") zip Iterable(spec.arg1 map normalize, spec.rel map normalize, spec.arg2 map normalize, spec.arg1Types, spec.arg2Types, spec.arg1Entity, spec.arg2Entity)).flatMap {
      case (a, b) => if (b.isEmpty) {
        None
      } else {
        Some("+" + a, b.get)
      }
    }
    val queryText = parts.map { case (field, value) => field + ":\"" + value + "\"" }.mkString(" ")

    Logger.debug("SOLR query: " + queryText)
    squery.setQuery(queryText + " size:[50 TO *]")
    squery.setSort("size", SolrQuery.ORDER.desc)
    squery.setRows(1000)
    squery.setTimeAllowed(Executor.queryTimeout)
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
        for (result <- response.getResults().asScala) yield {
          val bytes = result.getFieldValue("instances").asInstanceOf[Array[Byte]]
          val instances: List[Instance[ReVerbExtraction]] =
            kryo.invert(bytes)
              .getOrElse(throw new IllegalArgumentException("Could not deserialize instances: " + bytes.toSeq.toString))
              .asInstanceOf[List[Instance[ReVerbExtraction]]]

          val arg1 = ExtractionArgument(
            norm = result.getFieldValue("arg1_norm").asInstanceOf[String],
            entity = {
              if (!result.containsKey("arg1_entity_id")) None
              else {
                val id = result.getFieldValue("arg1_entity_id").asInstanceOf[String]
                val name = result.getFieldValue("arg1_entity_name").asInstanceOf[String]
                val inlink_ratio = result.getFieldValue("arg1_entity_inlink_ratio").asInstanceOf[Double]
                val score = result.getFieldValue("arg1_entity_score").asInstanceOf[Double]
                Some(FreeBaseEntity(name, id, score, inlink_ratio))
              }
            },
            types =
              if (!result.containsKey("arg1_fulltypes")) Set.empty
              else result.getFieldValue("arg1_fulltypes").asInstanceOf[java.util.List[String]].asScala.map(FreeBaseType.parse(_).get).toSet)

          val arg2 = ExtractionArgument(
            norm = result.getFieldValue("arg2_norm").asInstanceOf[String],
            entity = {
              if (!result.containsKey("arg2_entity_id")) None
              else {
                val id = result.getFieldValue("arg2_entity_id").asInstanceOf[String]
                val name = result.getFieldValue("arg2_entity_name").asInstanceOf[String]
                val inlink_ratio = result.getFieldValue("arg2_entity_inlink_ratio").asInstanceOf[Double]
                val score = result.getFieldValue("arg2_entity_score").asInstanceOf[Double]
                Some(FreeBaseEntity(name, id, score, inlink_ratio))
              }
            },
            types =
              if (!result.containsKey("arg2_fulltypes")) Set.empty
              else result.getFieldValue("arg2_fulltypes").asInstanceOf[java.util.List[String]].asScala.map(FreeBaseType.parse(_).get).toSet)

          val rel = ExtractionRelation(result.getFieldValue("rel_norm").asInstanceOf[String])

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
