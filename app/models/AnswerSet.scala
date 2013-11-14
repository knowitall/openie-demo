package models

import scala.collection.immutable
import edu.knowitall.common.enrich.Traversables._
import edu.knowitall.paraphrasing.Paraphrase
import edu.knowitall.paraphrasing.IdentityDerivation

/** The Answer set is a collection of the answers.
  *
  * @param  groups  the answers themselves
  * @param  filters  the filter tabs for the answers
  * @param  queryEntities  the entities associated with the singularly filled query position, or none
  */
case class AnswerSet (answers: List[Answer], filters: immutable.SortedSet[TypeFilterTab], queryEntities: immutable.List[(FreeBaseEntity, Int)], paraphraseHits: Seq[(Paraphrase, Int)]) {

  val answerCount = answers.size
  val resultsCount = answers.map(_.resultsCount).sum

  val attrs = answers.flatMap(_.attrs).toSet

  def exactPP(ppIndex: Int): AnswerSet = {
    
    // keep only results from paraphraseHits(ppIndex)._1
    val pp = paraphraseHits.lift(ppIndex).map(_._1).getOrElse {
      throw new IllegalArgumentException(s"Paraphrase index $ppIndex is not defined.")
    }
    def filterDGroup(dgroup: DerivationGroup): Option[DerivationGroup] = {
      if (dgroup.paraphrases.contains(pp)) {
        Some(dgroup.copy(paraphrases = Seq(pp)))
      } else {
        None
      }
    }
    def filterAnswer(answer: Answer): Answer = answer.copy(dgroups = answer.dgroups.flatMap(filterDGroup))
    
    val filteredAnswers = answers.map(filterAnswer).filterNot(_.resultsCount == 0)
    val typeFilters = TypeFilters.fromGroups(filteredAnswers, false)
    
    AnswerSet(filteredAnswers, 
        AnswerSet.typeFilterTabs(filteredAnswers, typeFilters), 
        AnswerSet.queryEntities(filteredAnswers),
        Seq(paraphraseHits(ppIndex)))    
  }
  
  def page(pageNumber: Int, pageSize: Int): AnswerSet =
    this.copy(answers=this.answers.drop(pageNumber * pageSize).take(pageSize))

  def filter(filters: Iterable[TypeFilter]) = {
    if (filters.isEmpty) this
    else this.copy(answers = answers filter (group =>
      filters.forall(filter => filter(group))
  ))}

//  val paraphraseHits: Seq[(Paraphrase, Int)] = {
//    // flatten out (paraphrase, ..., triple)
//    val ppTriples = for (
//        answer <- answers;
//        dgroup <- answer.dgroups;
//        paraphrase <- dgroup.ppsDeduped;
//        (query, triples) <- dgroup.queryTriples;
//        triple <- triples) yield (paraphrase, triple)
//
//    val ppHitsMap = ppTriples.groupBy(_._1).map { case (pp, pphits) => (pp, pphits.size) }
//    val sortedHits = ppHitsMap.iterator.toSeq.sortBy { case (pp, hits) => DerivationGroup.ppDerivationSort(pp) }
//    sortedHits.filterNot(_._1.derivation.equals(IdentityDerivation))
//  }
}

object AnswerSet {
  
  def typeFilterTabs(answers: Seq[Answer], filters: Seq[TypeFilter]) = {
    immutable.SortedSet.empty[TypeFilterTab] ++ filters.map(filter => TypeFilterTab(filter, answers.count(answer => filter(answer))))
  }
  
  def queryEntities(answers: Seq[Answer]): List[(FreeBaseEntity, Int)] = {
    answers.flatMap(_.queryEntity).
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
  }
  
  def from(answers: Seq[Answer], filters: Seq[TypeFilter]) = { 

    val paraphraseHits: Seq[(Paraphrase, Int)] = {
      // flatten out (paraphrase, ..., triple)
      val ppTriples = for (
          answer <- answers;
          dgroup <- answer.dgroups;
          paraphrase <- dgroup.ppsDeduped;
          (query, triples) <- dgroup.queryTriples;
          triple <- triples) yield (paraphrase, triple)

    val ppHitsMap = ppTriples.groupBy(_._1).map { case (pp, pphits) => (pp, pphits.size) }
    val sortedHits = ppHitsMap.iterator.toSeq.sortBy { case (pp, hits) => DerivationGroup.ppDerivationSort(pp) }
    sortedHits.filterNot(_._1.derivation.equals(IdentityDerivation))
  }

    AnswerSet(
      // we need to re-apply the query filters because some entities may have been
      // unlinked due to a low confidence.
      answers.toList,
      typeFilterTabs(answers, filters),
      queryEntities(answers), 
      paraphraseHits)
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
