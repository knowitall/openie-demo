package models

import edu.knowitall.parsing.QuestionParser

import controllers.DemoComponents

/**
 * A query is a question parser and its input.
 * 
 * For easy serialization, the parser is represented by its name as a
 * key in the components mapping.
 */
trait Query {

  def question: String
  def parserName: String
  
  require(DemoComponents.parsers.contains(parserName), parserName + " is not a valid parser name.")
  
  val parser = DemoComponents.parsers(parserName)
}