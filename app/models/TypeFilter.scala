package models

import edu.washington.cs.knowitall.browser.extraction.FreeBaseType
import edu.washington.cs.knowitall.common.enrich.Traversables._

case class TypeFilter(typ: FreeBaseType) {
  def displayName = typ.typ.replaceAll("_", " ")

  def apply(answer: GroupTitlePart): Boolean =
    answer.types.exists( typ =>
      typ == this.typ
    )
}

object TypeFilters {
  final val MINIMUM_OCCURRENCE = 2

  def fromGroups(groups: Iterable[Group]) = {
    // build all possible filters
    val it = for {
      group <- groups
      part <- group.title.parts
      typ <- part.types
      if typ.domain != "base" && typ.domain != "user"
    } yield (TypeFilter(typ))

    val ordered = it.histogram.filter { case (filter, count) =>
      count > MINIMUM_OCCURRENCE
    }.toSeq.sortBy(-_._2).map(_._1)

    ordered
  }
}
