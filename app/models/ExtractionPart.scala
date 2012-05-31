package models

sealed abstract class ExtractionPart
case object Argument1 extends ExtractionPart
case object Argument2 extends ExtractionPart
case object Relation extends ExtractionPart
