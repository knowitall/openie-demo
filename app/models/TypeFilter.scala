package models

import edu.washington.cs.knowitall.browser.extraction.FreeBaseType
import edu.washington.cs.knowitall.common.enrich.Traversables._

case class TypeFilter(base: String) {
  def displayName = base

  def apply(answer: GroupTitlePart): Boolean =
    answer.types.exists( typ =>
      TypeFilter.baseName(typ.name) == this.base
    )
}

object TypeFilter {
  def baseOfFreebaseType(typ: FreeBaseType) = {
    TypeFilter(baseName(typ.name))
  }

  private def baseName(typ: String) = typ.dropWhile(_ == '/').takeWhile(_ != '/')
}

object TypeFilters {
  final val MINIMUM_OCCURRENCE = 2

  def fromGroups(groups: Iterable[Group]) = {
    // build all possible filters
    val it = for {
      group <- groups
      part <- group.title.parts
      typ <- part.types
      if !typ.name.startsWith("/base/") && !typ.name.startsWith("/user/")
    } yield (TypeFilter.baseOfFreebaseType(typ))

    val ordered = it.histogram.filter { case (filter, count) =>
      count > MINIMUM_OCCURRENCE
    }.toSeq.sortBy(-_._2).map(_._1)

    ordered
  }
}