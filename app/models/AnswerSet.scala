package models

import scala.collection.immutable
import edu.knowitall.openie.models.FreeBaseEntity
import edu.knowitall.common.enrich.Traversables._

/** The Answer set is a collection of the answers.
  *
  * @param  groups  the answers themselves
  * @param  filters  the filter tabs for the answers
  * @param  queryEntities  the entities associated with the singularly filled query position, or none
  */
case class AnswerSet(answers: Seq[Answer], filters: immutable.SortedSet[TypeFilterTab], queryEntities: immutable.List[(FreeBaseEntity, Int)]) {
  def answerCount = answers.size
  def sentenceCount = answers.iterator.map(_.contents.size).sum

  def page(pageNumber: Int, pageSize: Int): AnswerSet =
    this.copy(answers=this.answers.drop(pageNumber * pageSize).take(pageSize))

  def filter(filters: Iterable[TypeFilter]) = {
    if (filters.isEmpty) this
    else this.copy(answers = answers filter (group =>
      filters.forall(filter => filter(group.title))
  ))}
}

object AnswerSet {
  def from(query: TripleQuery, answers: Seq[Answer], filters: Seq[TypeFilter]) = {
    val filteredGroups = answers filter (answer => query.filters forall (filter => filter(answer.title)))
    val filterTabs = immutable.SortedSet.empty[TypeFilterTab] ++ filters.map(filter => TypeFilterTab(filter, answers.count(answer => filter(answer.title))))

    // all the queryEntities from answer
    def queryEntities() = answers.flatMap(_.queryEntity).
      // sum the counts of the same entities
      mergeHistograms.
      // group by fbid to avoid different confidence links
      groupBy(_._1.fbid).
      // take the highest confidence link from each fbid category
      map { case (fbid, seq) => (seq.iterator.map(_._1).maxBy(_.score), seq.iterator.map(_._2).sum) }.
      // for sorting
      toList.
      // (entity, total size)
      sortBy(_._2)(Ordering[Int].reverse)

    this(
      // we need to re-apply the query filters because some entities may have been
      // unlinked due to a low confidence.
      filteredGroups,
      filterTabs,
      queryEntities())
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
