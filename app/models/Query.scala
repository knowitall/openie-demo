package models

import edu.knowitall.parsing.QuestionParser

import controllers.DemoComponents

/**
 * A query is a question parser and its input.
 *
 * For easy serialization, the parser is represented by its name as a
 * key in the components mapping.
 */
class Query(val question: String, val paraphraserName: String, val parserName: String) {

  def paraphraser = {
    require(DemoComponents.paraphrasers.contains(paraphraserName), paraphraserName + " is not a valid paraphraser name.")
    DemoComponents.paraphrasers(paraphraserName)
  }

  def parser = {
    require(DemoComponents.parsers.contains(parserName), parserName + " is not a valid parser name.")
    DemoComponents.parsers(parserName)
  }

  override def toString: String = s"Query($question, $paraphraserName, $parserName)"
}

object Query {


  def unapply(query: Query): Option[(String, String, String)] = Some(query.question, query.paraphraserName, query.parserName)

  def apply(q: String, pphraser: String, parser: String) = new Query(q, pphraser, parser)
}