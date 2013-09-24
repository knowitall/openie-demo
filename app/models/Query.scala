package models

import edu.knowitall.parsing.QuestionParser

import controllers.DemoComponents

/**
 * A query is a question parser and its input.
 * 
 * For easy serialization, the parser is represented by its name as a
 * key in the components mapping.
 */
class Query(val question: String, val parserName: String) {

  def parser = {
    require(DemoComponents.parsers.contains(parserName), parserName + " is not a valid parser name.")
    DemoComponents.parsers(parserName)
  }
}

object Query {
  
  
  def unapply(query: Query): Option[(String, String)] = Some(query.question, query.parserName)
  
  def apply(q: String, p: String) = new Query(q, p)
}