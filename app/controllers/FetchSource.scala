package controllers

import edu.knowitall.common.Timing
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.tokenize.OpenNlpTokenizer
import play.api.Logger
import models.Query
import models.Answer

abstract class FetchSource {
  
  def fetch(query: Query): Executor.Result[Answer]
}

object Fetch {
  val tokenizer = {
    Timing.timeThen {
      new OpenNlpTokenizer()
    } { ns => Logger.info("Initialized OpenNlpTokenizer (" + Timing.Seconds.format(ns) + ")") }
  }

  def normalize(string: String) = {
    val tokenized = tokenizer.synchronized {
      Fetch.tokenizer.tokenize(string)
    }

    (tokenized.map(_.string) map MorphaStemmer.lemmatize).mkString(" ")
  }
}