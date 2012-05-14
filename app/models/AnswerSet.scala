package models

case class AnswerSet(groups: Seq[Group]) {
  def answerCount = groups.size
  def sentenceCount = groups.iterator.map(_.contents.size).sum
}