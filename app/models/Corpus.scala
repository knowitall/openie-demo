package models

case class Corpus(val displayName: String, val indexName: String)

object Corpus {
  val corpora = List(
    Corpus("Google", "g1b"),
    Corpus("ClueWeb", "cw"),
    Corpus("News", "news"),
    Corpus("Nell", "nell"),
    Corpus("Wikipedia", "wiki")
  )
}
