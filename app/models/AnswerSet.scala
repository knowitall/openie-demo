package models

import scala.collection.immutable
import org.apache.http.annotation.Immutable
import edu.washington.cs.knowitall.browser.extraction.FreeBaseEntity
import edu.washington.cs.knowitall.common.enrich.Traversables._

/** The Answer set is a collection of the answers.
  * 
  * @param  groups  the answers themselves
  * @param  filters  the filter tabs for the answers
  * @param  queryEntities  the entities associated with the singularly filled query position, or none
  */
case class AnswerSet(groups: Seq[Answer], filters: immutable.SortedSet[TypeFilterTab], queryEntities: immutable.List[(FreeBaseEntity, Int)]) {
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
  def from(query: Query, answers: Seq[Answer], filters: Seq[TypeFilter]) = {
    val filteredGroups = answers filter (answer => query.filters forall (filter => filter(answer.title)))
    val filterTabs = immutable.SortedSet.empty[TypeFilterTab] ++ filters.map(filter => TypeFilterTab(filter, answers.count(answer => filter(answer.title))))
    
    val queryEntities = answers.flatMap(_.queryEntity).groupBy(_.fbid).toList.sortBy(_._2.size)(Ordering[Int].reverse).map { group =>
      (group._2.maxBy(_.score), group._2.size)
    }
 
    this(
      // we need to re-apply the query filters because some entities may have been
      // unlinked due to a low confidence.
      filteredGroups,
      filterTabs,
      queryEntities)
  }
}

case class TypeFilterTab(filter: TypeFilter, count: Int)
extends Ordered[TypeFilterTab] {
  override def compare(that: TypeFilterTab): Int = {
    val compare = -this.count.compareTo(that.count)
    if (compare != 0) compare
    else filter.name.compare(that.filter.name)
  }
}