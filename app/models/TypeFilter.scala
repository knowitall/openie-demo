package models

import edu.knowitall.openie.models.FreeBaseType
import edu.knowitall.common.enrich.Traversables.traversableOnceTo
import edu.knowitall.common.Resource.using
import scala.io.Source

sealed abstract trait TypeFilter {
  def name: String
  def displayName: String
  def parts: List[ExtractionPart]
  def apply(answer: AnswerTitle): Boolean
}

case class PositiveTypeFilter(val typ: FreeBaseType, override val parts: List[ExtractionPart]) extends TypeFilter {
  def name = typ.name
  def displayName = typ.typ.replaceAll("_", " ")

  def apply(answer: AnswerTitle): Boolean =
    answer.parts exists (part => (parts contains part.extractionPart) &&
      part.types.contains(this.typ)
    )
}

case class PositiveStringTypeFilter(val string: String, override val parts: List[ExtractionPart]) extends TypeFilter {
  def name = string
  def displayName = string.replaceAll("_", " ")

  def apply(answer: AnswerTitle): Boolean =
    answer.parts exists (part => (parts contains part.extractionPart) &&
      part.types.exists(_.typ equalsIgnoreCase this.string)
    )
}

case class NegativeTypeFilter(val typ: FreeBaseType, override val parts: List[ExtractionPart]) extends TypeFilter {
  def name = typ.name
  def displayName = typ.typ.replaceAll("_", " ")

  def apply(answer: AnswerTitle): Boolean =
    answer.parts forall (part => (parts contains part.extractionPart) &&
      !part.types.contains(this.typ)
    )
}

object TypeFilters {
  final val MINIMUM_OCCURRENCE = 2
  final val MAXIMUM_FILTER_COUNT = 25

  val blacklist: Set[FreeBaseType] =
    using (this.getClass.getResource("/type_blacklist.txt").openStream()) { stream =>
      using (Source.fromInputStream(stream)) { source =>
        (source.getLines flatMap FreeBaseType.parse).toSet
      }
    }

  val weights: Map[FreeBaseType, Int] =
    using (this.getClass.getResource("/type_weights.txt").openStream()) { stream =>
      using (Source.fromInputStream(stream)) { source =>
        source.getLines.map { line =>
          val Array(fb, weight) = line.split("\t")
          (FreeBaseType.parse(fb).get, weight.toInt)
        }.toMap
      }
    }

  class EnrichedFreeBaseType(fb: FreeBaseType) {
    def valid = {
      fb.domain != "base" && fb.domain != "user" &&
      !fb.typ.isEmpty() && !blacklist.contains(fb)
    }

    def weight = weights.getOrElse(fb, 0)
  }

  implicit def enrichFreeBaseType(fb: FreeBaseType) = new EnrichedFreeBaseType(fb)

  def fromGroups(query: Query, groups: Iterable[Answer], debug: Boolean): Seq[TypeFilter] = {
    if (query.full) List.empty
    else {
      // build all possible filters
      val it = for {
        group <- groups

        // only use filters for "free" parts
        part <- group.title.parts
        if (query.freeParts.contains(part.extractionPart))

        // avoid user-submitted type categories
        typ <- part.types
        if typ.valid
      } yield (PositiveTypeFilter(typ, query.freeParts))

      // order the filters and take the top few
      val ordered = it.histogram.filter {
        case (filter, count) =>
          count > MINIMUM_OCCURRENCE
      }.toList.sortBy(-_._2).map(_._1)

      if (debug) ordered
      else {
        val grouped = ordered.take(MAXIMUM_FILTER_COUNT).map(filter =>
          (filter, groups.toSet filter
            (group => filter(group.title))))

        // remove groups that are a proper subset of another
        val filtered = grouped filter { case (filter, groups) =>
          !grouped.exists { case (otherFilter, otherGroups) =>
            !(filter eq otherFilter) &&
              otherGroups.size > groups.size &&
              groups.forall(otherGroups.contains) &&
              groups.size > 9 * otherGroups.size / 10
          }
        }

        filterOverlappingTypeFilters(grouped).map(_._1)
      }
    }
  }

  def filterOverlappingTypeFilters(types: List[(TypeFilter, Set[Answer])]) = {
    val THRESHOLD = 0.9
    types.foldLeft(List.empty[(TypeFilter, Set[Answer])]) { case (acc, next@(tf, ans)) =>
      def overlapped =
        acc exists { case (tfx, ansx) =>
          (ans intersect ansx).size > THRESHOLD * ansx.size
        }

      if (overlapped) acc
      else next :: acc
    }.reverse
  }
}
