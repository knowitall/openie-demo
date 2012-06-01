package models

import scala.collection.immutable
import org.apache.http.annotation.Immutable

case class AnswerSet(groups: Seq[Group], filters: immutable.SortedSet[TypeFilterTab]) {
  def answerCount = groups.size
  def sentenceCount = groups.iterator.map(_.contents.size).sum

  def page(pageNumber: Int, pageSize: Int): AnswerSet =
    this.copy(groups=this.groups.drop(pageNumber * pageSize).take(pageSize))

  def filter(filters: Iterable[TypeFilter]) = {
    if (filters.isEmpty) this
    else this.copy(groups = groups filter (group =>
      filters.forall(filter => filter(group.title))
  ))}
}

object AnswerSet {
  def from(groups: Seq[Group], filters: Seq[TypeFilter]) = {
    this(groups, immutable.SortedSet.empty[TypeFilterTab] ++ filters.map(filter => TypeFilterTab(filter, groups.count(group => filter(group.title)))))
  }
}

case class TypeFilterTab(filter: TypeFilter, count: Int)
extends Ordered[TypeFilterTab] {
  override def compare(that: TypeFilterTab): Int = {
    val compare = -this.count.compareTo(that.count)
    if (compare != 0) compare
    else filter.typ.typ.compare(that.filter.typ.typ)
  }
}