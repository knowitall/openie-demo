package edu.knowitall.common.enrich

import edu.knowitall.collection.immutable.Bag

object Traversables {
  implicit def traversableOnceTo[T](as: TraversableOnce[T]): SuperTraversableOnce[T] = new SuperTraversableOnce[T](as)

  implicit def traversableOncePairIntTo[T](as: TraversableOnce[(T, Int)]): SuperTraversableOncePairInt[T] = new SuperTraversableOncePairInt[T](as)

  implicit def traversableOncePairTo[T, U](as: TraversableOnce[(T, U)]): SuperTraversableOncePair[T, U] = new SuperTraversableOncePair[T, U](as)
}

sealed class SuperTraversableOnce[T](value: TraversableOnce[T]) {
  def histogram: Map[T, Int] = {
    value.foldLeft(Map[T, Int]()) { (m, c) =>
      m.updated(c, m.getOrElse(c, 0) + 1)
    }
  }
}

sealed class SuperTraversableOncePairInt[T](value: TraversableOnce[(T, Int)]) {
  import Traversables._
  def mergeHistograms: Map[T, Int] = value.mergeKeys(_ + _)
}

sealed class SuperTraversableOncePair[T, U](value: TraversableOnce[(T, U)]) {
  def mergeKeys(merge: (U, U) => U): Map[T, U] = {
    value.foldLeft(Map[T, U]()) {
      case (map, (k, v)) =>
        map + (k -> map.get(k).map(merge(_, v)).getOrElse(v))
    }
  }

  def toListMultimap: Map[T, List[U]] = {
    value.foldLeft(Map[T, List[U]]().withDefaultValue(List.empty[U])) {
      case (map, (k, v)) =>
        map + (k -> (v :: map(k)))
    }
  }

  def toSetMultimap: Map[T, Set[U]] = {
    value.foldLeft(Map[T, Set[U]]().withDefaultValue(Set.empty[U])) {
      case (map, (k, v)) =>
        map + (k -> (map(k) + v))
    }
  }

  def toBagMultimap: Map[T, Bag[U]] = {
    value.foldLeft(Map[T, Bag[U]]().withDefaultValue(Bag.empty[U])) {
      case (map, (k, v)) =>
        val bag = map(k)
        map + (k -> (bag + v))
    }
  }
}
