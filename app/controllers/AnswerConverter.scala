package controllers

import models.{FreeBaseEntity, FreeBaseType}
import edu.knowitall.execution.Search.{Field, arg1, rel, arg2}
import edu.knowitall.execution.Tuple
import edu.knowitall.execution.AnswerDerivation
import edu.knowitall.scoring.ScoredAnswerGroup
import java.util.ArrayList
import scala.collection.JavaConversions._
import models.AnswerPart
import org.apache.solr.client.solrj.SolrServer
import org.apache.solr.client.solrj.SolrQuery
import org.apache.solr.common.SolrDocument
import models.Content
import edu.knowitall.collection.immutable.Interval
import models.Answer
import play.api.Logger

case class Link(entity: FreeBaseEntity, types: Set[FreeBaseType])

class AnswerConverter(solr: SolrServer) {

  import AnswerConverter._

  /**
   * Todo: factor out combining code better,
   * and properly merge all answerparts.
   */
  def getAnswers(sags: Seq[ScoredAnswerGroup]): Seq[Answer] = {
    val answers = sags map getAnswer
    val titleGroups = answers.groupBy(_.title)
    def combine(answers: Seq[Answer]): Answer = {
      val allQEs = answers.flatMap(_.queryEntity)
      val groupedQEs = allQEs.groupBy(_._1)
      val combinedQEs = groupedQEs.map { case (entity, qes) => (entity, qes.map(_._2).sum) }
      val sortedCombinedQEs = combinedQEs.toSeq.sortBy(-_._2).toList
      Answer(answers.head.parts, answers.flatMap(_.contents), sortedCombinedQEs)
    }
    val combinedAnswers = titleGroups.map { case (title, answers) => (title, combine(answers)) }
    val sortedAnswers = combinedAnswers.values.toSeq.sortBy(-_.contents.size)
    sortedAnswers
  }

  def getAnswer(sag: ScoredAnswerGroup): Answer = {
    val answerParts = sag.answer.indices.map(i => getAnswerPart(i, sag))
    val contents = sag.derivations.flatMap(getContents)
    val queryEntities = getNonAnswerEntities(sag.derivations)
    Answer(answerParts, contents, queryEntities)
  }

  private val sourceIdRegex = """\w+\.source_ids""".r

  def getContents(deriv: AnswerDerivation): Seq[Content] = {
    val tuple = deriv.etuple.tuple
    val sourceAttrs = tuple.attrs.keySet.filter(k => sourceIdRegex.pattern.matcher(k).matches)
    val sourceIds = sourceAttrs.toSeq.flatMap(tuple.get).flatMap(_.asInstanceOf[List[String]])
    val docs = sourceIds.par.flatMap(getSolrDoc)
    val contents = docs.map(getContent)
    contents.toList
  }

  def getSolrDoc(id: String): Option[SolrDocument] = {

    // make the solr query
    val idQueryString = s"id:$id"
    val solrQuery = new SolrQuery(idQueryString)
    val response = solr.query(solrQuery)
    val docs = response.getResults()

    if (docs.size == 0) {
      Logger.error("Metadata doc not found for id: " + id)
      None
    } else {
      if (docs.size > 1) Logger.warn("Should only be one metadata doc per id, found %d docs for id %s".format(docs.size, id))
      Some(docs.get(0))
    }
  }

  def getContent(doc: SolrDocument): Content = {

    def getString(field: String) = doc.getFieldValue(field).asInstanceOf[String]
    def getInterval(field: String) = Interval.deserialize(getString(field))
    val arg1Interval = getInterval("arg1_interval")
    val relInterval  = getInterval("rel_interval")
    val arg2Interval = getInterval("arg2_interval")
    val urlString = getString("url")
    val tokens  = getString("sentence_text").split(" ").toList
    val rel = relInterval.map(i => tokens(i)).mkString(" ")
    val confidence = doc.getFieldValue("confidence").asInstanceOf[Double]
    val corpus = getString("corpus")
    Content(tokens, urlString, List(arg1Interval, arg2Interval), rel, confidence, corpus)
  }
}

object AnswerConverter {

   private val linkFieldTemplates = Map(
      "id"     -> "_entity_id",
      "name"   -> "_entity_name",
      "iratio" -> "_entity_inlink_ratio",
      "score"  -> "_entity_score",
      "types"  -> "_fulltypes")

  private def getLink(prefix: String, field: Field, tuple: Tuple): Option[Link] = {
    val fullPrefix = prefix + "." + field.name
    val fullFields = linkFieldTemplates.map { case (name, template) => (name, fullPrefix+template) }
    val vOpts = fullFields.map { case (name, field) => (name, tuple.get(field)) }
    if (!vOpts.values.forall(_.isDefined)) None
    else {
      val vs = vOpts.map { case (name, vOpt) => (name, vOpt.get) }
      val entity = FreeBaseEntity(
          vs("name").asInstanceOf[String],
          vs("id").asInstanceOf[String],
          vs("score").asInstanceOf[Double],
          vs("iratio").asInstanceOf[Double])
      val typesList = vs("types").asInstanceOf[List[String]]
      val types = typesList.toSet.flatMap(FreeBaseType.parse)
      val link = Link(entity, types)
      Some(link)
    }
  }

  private val attrRegex = """(\w+)\.(arg1|rel|arg2)""".r

  private def getPrefixAndField(attr: String): Option[(String, Field)] = {
    attr match {
      case attrRegex(prefix, fieldString) => fieldString match {
        case "arg1" => Some((prefix, arg1))
        case "arg2" => Some((prefix, arg2))
        case "rel"  => None
        case _ => throw new RuntimeException("Unknown link field: "+fieldString)
      }
      case _ => throw new RuntimeException("Unknown attr string: "+attr)
    }
  }

  private def getNonAnswerEntities(derivs: Seq[AnswerDerivation]): List[(FreeBaseEntity, Int)] = {
    val allEntities = derivs.flatMap(getNonAnswerLinks)
    val entityGroups = allEntities.groupBy(_.entity)
    val highestScoreCount: Iterator[(FreeBaseEntity, Int)] = entityGroups.iterator.map { case (_, links) =>
      val highestScore = links.map(_.entity).maxBy(_.score)
      val count = links.size
      (highestScore, count)
    }
    highestScoreCount.toList.sortBy(-_._2)
  }

  private def getNonAnswerLinks(deriv: AnswerDerivation): Seq[Link] = {
    // get attrs that aren't part of the answer
    val allAttrs = deriv.etuple.tuple.attrs.keySet
    val answerAttrs = deriv.attrs.toSet
    val nonAnswerAttrs = allAttrs &~ answerAttrs
    // get non-answer attrs that are either arg1/arg2
    val nonAnswerTripleParts = nonAnswerAttrs.flatMap { attr =>
      attr match {
        case attrRegex(prefix, "arg1") => Some((prefix, arg1))
        case attrRegex(prefix, "arg2") => Some((prefix, arg2))
        case _ => None
      }
    }
    // get any links for these parts
    val links = nonAnswerTripleParts.flatMap { case (prefix, field) =>
      getLink(prefix, field, deriv.etuple.tuple)
    }
    links.toSeq
  }

  private def getAllLinks(part: Int, derivs: Seq[AnswerDerivation]): Seq[Link] = {

    require(derivs.forall(deriv => part >= 0 && part < deriv.answer.length), "part must specify a valid answer part index for all derivations")

    /**
     * Get the attr associated with the answer part for each tuple
     * Then group tuples by these attrs
     * Then combine links from these tuples over their attrs
     * and done.
     */
    val attrGroups = derivs.groupBy(_.attrs(part))
    attrGroups.toSeq.flatMap { case (attr, ds) =>
      val tuples = ds.map(_.etuple.tuple)
      tuples.flatMap { t =>
        getPrefixAndField(attr).flatMap { case (prefix, field) =>
          getLink(prefix, field, t)
        }
      }
    }
  }

  private def getAnswerPart(part: Int, sag: ScoredAnswerGroup): AnswerPart = {
    // "lemma"
    val lemma = sag.answer(part)

    val attrs = sag.derivations.map(_.attrs(part)).toSet

    val synonyms = sag.alternates.map(_(part))

    val links = getAllLinks(part, sag.derivations)

    val sortedLinks = links.groupBy(_.entity).toSeq.sortBy(-_._2.size)

    val topLink = sortedLinks.headOption.flatMap(_._2.headOption)

    val entity = topLink.map(_.entity)

    val types = topLink.map(_.types).getOrElse(Set.empty)

    AnswerPart(lemma, attrs, synonyms, entity, types)
  }
}