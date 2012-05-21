package models

case class AnswerSet(groups: Seq[Group], filters: Seq[(TypeFilter, Int)]) {
  def answerCount = groups.size
  def sentenceCount = groups.iterator.map(_.contents.size).sum

  def page(pageNumber: Int, pageSize: Int): AnswerSet =
    this.copy(groups=this.groups.drop(pageNumber * pageSize).take(pageSize))

  def filter(filters: Iterable[TypeFilter]) = {
    if (filters.isEmpty) this
    else this.copy(groups = groups filter (group =>
      group.title.parts.exists(part => filters.forall(_(part)))
  ))}
}

object AnswerSet {
  def from(groups: Seq[Group], filters: Seq[TypeFilter]) = {
    this(groups, filters.map(filter => (filter, groups.count(group => group.title.parts.exists(part => filter(part))))))
  }
}
