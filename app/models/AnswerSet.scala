package models

case class AnswerSet(groups: Seq[Group], typeFilters: Seq[TypeFilter]) {
  def answerCount = groups.size
  def sentenceCount = groups.iterator.map(_.contents.size).sum

  def page(pageNumber: Int, pageSize: Int): AnswerSet =
    this.copy(groups=this.groups.drop(pageNumber * pageSize).take(pageSize))

  def filter(filters: Iterable[TypeFilter]) = {
    if (filters.isEmpty) this
    else this.copy(groups = groups filter (group =>
      group.title.parts.exists(part => filters.exists(_(part)))
  ))}
}
