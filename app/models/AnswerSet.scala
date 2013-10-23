package models

import scala.collection.immutable
import edu.knowitall.common.enrich.Traversables._
import edu.knowitall.paraphrasing.Paraphrase

/** The Answer set is a collection of the answers.
  *
  * @param  groups  the answers themselves
  * @param  filters  the filter tabs for the answers
  * @param  queryEntities  the entities associated with the singularly filled query position, or none
  */
case class AnswerSet(answers: Seq[Answer], filters: immutable.SortedSet[TypeFilterTab], queryEntities: immutable.List[(FreeBaseEntity, Int)]) {
  def answerCount = answers.size
  def resultsCount = answers.map(_.resultsCount).sum

  val attrs = answers.flatMap(_.attrs).toSet

  def page(pageNumber: Int, pageSize: Int): AnswerSet =
    this.copy(answers=this.answers.drop(pageNumber * pageSize).take(pageSize))

  def filter(filters: Iterable[TypeFilter]) = {
    if (filters.isEmpty) this
    else this.copy(answers = answers filter (group =>
      filters.forall(filter => filter(group))
  ))}

  val paraphraseHits: Seq[(Paraphrase, Int)] = {
    // flatten out (paraphrase, ..., triple)
    val ppTriples = for (
        answer <- answers;
        dgroup <- answer.dgroups;
        paraphrase <- dgroup.paraphrases;
        (query, triples) <- dgroup.queryTriples;
        triple <- triples) yield (paraphrase, triple)

    val ppHitsMap = ppTriples.groupBy(_._1).map { case (pp, pphits) => (pp, pphits.size) }
    ppHitsMap.iterator.toSeq.sortBy(_._1.derivation.score)
  }
}

object AnswerSet {
  def from(answers: Seq[Answer], filters: Seq[TypeFilter]) = {
    val filterTabs = immutable.SortedSet.empty[TypeFilterTab] ++ filters.map(filter => TypeFilterTab(filter, answers.count(answer => filter(answer))))

    // all the queryEntities from answer
    val queryEntities = answers.flatMap(_.queryEntity).
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

    AnswerSet(
      // we need to re-apply the query filters because some entities may have been
      // unlinked due to a low confidence.
      answers,
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
