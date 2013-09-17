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
import models.Answer
import edu.knowitall.openie.models.Extraction
import edu.knowitall.openie.models.ExtractionCluster

abstract class FetchSource {
  
  def fetch(query: Query): Executor.Result[Answer]
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