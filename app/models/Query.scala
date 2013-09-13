package models

import edu.knowitall.parsing.QuestionParser

trait Query {

  def question: String
  def parser: QuestionParser
}