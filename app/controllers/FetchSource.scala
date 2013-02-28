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
import edu.washington.cs.knowitall.browser.extraction.ExtractionArgument
import edu.washington.cs.knowitall.browser.extraction.ExtractionGroup
import edu.washington.cs.knowitall.browser.extraction.ExtractionRelation
import edu.washington.cs.knowitall.browser.extraction.FreeBaseEntity
import edu.washington.cs.knowitall.browser.extraction.FreeBaseType
import edu.washington.cs.knowitall.browser.extraction.Instance
import edu.washington.cs.knowitall.browser.extraction.ReVerbExtraction
import edu.washington.cs.knowitall.browser.lucene
import edu.washington.cs.knowitall.browser.lucene.LuceneFetcher
import edu.washington.cs.knowitall.browser.lucene.QuerySpec
import edu.washington.cs.knowitall.common.Resource.using
import edu.washington.cs.knowitall.common.Timing
import edu.washington.cs.knowitall.tool.stem.MorphaStemmer
import edu.washington.cs.knowitall.tool.tokenize.OpenNlpTokenizer
import play.api.Logger
import play.libs.Akka

sealed abstract class FetchSource

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
}
case object ActorSource extends FetchSource {
  lazy val fetcher = TypedActor(Akka.system).typedActorOf(TypedProps[LuceneFetcher](), Akka.system.actorFor("akka://openie-lucene-server@reliable.cs.washington.edu:9002/user/fetcher"))
}
case object SolrSource extends FetchSource {
  val solr = new HttpSolrServer("http://reliable.cs.washington.edu:8983/solr")
  solr.setSoTimeout(20000); // socket read timeout
  solr.setConnectionTimeout(20000);
  solr.setDefaultMaxConnectionsPerHost(100);
  solr.setMaxTotalConnections(100);
  solr.setFollowRedirects(false); // defaults to false
  solr.setAllowCompression(true);
  solr.setMaxRetries(1); // defaults to 0.  > 1 not recommended.

  def fetch(spec: QuerySpec) = {
    val squery = new SolrQuery()

    def normalize(string: String) = {
      val tokenized = Fetch.tokenizer.synchronized {
        Fetch.tokenizer.tokenize(string)
      }

      (tokenized.map(_.string) map MorphaStemmer.instance.lemmatize).mkString(" ")
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
    squery.setSortField("size", SolrQuery.ORDER.desc)
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

    val groups = for (result <- response.getResults().iterator().asScala) yield {
      val instances = using(new ByteArrayInputStream(result.getFieldValue("instances").asInstanceOf[Array[Byte]])) { is =>
        using(new ObjectInputStream(is) {
          override def resolveClass(desc: java.io.ObjectStreamClass): Class[_] = {
            try { Class.forName(desc.getName, false, getClass.getClassLoader) }
            catch { case ex: ClassNotFoundException => super.resolveClass(desc) }
          }
        }) { ois =>
          ois.readObject().asInstanceOf[List[Instance[ReVerbExtraction]]]
        }
      }

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

    val (ns, list) = Timing.time {
      groups.toList
    }

    Logger.debug("groups retrieved: " + list.size + " (" + Timing.Seconds.format(ns) + ")")
    lucene.Success(list)
  }
}