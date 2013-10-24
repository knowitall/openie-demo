package controllers

import models.{AbstractType, FreeBaseEntity, FreeBaseType}
import edu.knowitall.execution.Search.{Field, arg1, rel, arg2}
import edu.knowitall.execution._
import edu.knowitall.apps.AnswerDerivation
import edu.knowitall.scoring.ScoredAnswerGroup
import java.util.ArrayList
import scala.collection.JavaConversions._
import models.AnswerPart
import org.apache.solr.client.solrj.SolrServer
import org.apache.solr.client.solrj.SolrQuery
import org.apache.solr.common.SolrDocument
import models.DerivationGroup
import models.{Triple, DefaultTriple, OpenIETriple}
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
    val combinedAnswers = titleGroups.map { case (title, answers) => (title, combineAnswers(answers)) }
    val sortedAnswers = combinedAnswers.values.toSeq.sortBy(-_.resultsCount)
    sortedAnswers
  }

  def combineAnswers(answers: Seq[Answer]): Answer = {
    def combineQueryEntities: List[(FreeBaseEntity, Int)] = {
      val allQEs = answers.flatMap(_.queryEntity)
      val groupedQEs = allQEs.groupBy(_._1).map { case (entity, entityCounts) => (entity, entityCounts.map(_._2)) }
      val combinedQEs = groupedQEs.map { case (entity, counts) => (entity, counts.sum) }
      combinedQEs.toSeq.sortBy(-_._2).toList
    }
    def combineAnswerParts(parts: Seq[AnswerPart]): AnswerPart = {
      // take the most common lemma
      val topLemma = parts.map(_.lemma).groupBy(identity).iterator.maxBy(_._2.size)._1
      val allAttrs = parts.flatMap(_.attrs).toSet
      val allSynonyms = parts.flatMap(_.synonyms).distinct
      val allEntityTypes = parts.flatMap { p => p.entity.map(pe => (pe, p.types)) }
      val (bestEntity, bestTypes) = {
        if (allEntityTypes.isEmpty) (None, Set.empty[AbstractType])
        else {
          val (be, typs) = allEntityTypes.maxBy(_._1.score)
          (Some(be), typs)
        }
      }
      AnswerPart(topLemma, allAttrs, allSynonyms, bestEntity, bestTypes.toSeq)
    }

    def combineAllAnswerParts: Seq[AnswerPart] = {
      val maxParts = answers.map(_.parts.size).max
      val partsByIndex = (0 until maxParts).map { index =>
        answers.flatMap(_.parts.lift(index))
      }
      partsByIndex.map(combineAnswerParts)
    }

    Answer(combineAllAnswerParts, DerivationGroup.dedupe(DerivationGroup.regroup(answers.flatMap(_.dgroups))), combineQueryEntities)
  }

  def getAnswer(sag: ScoredAnswerGroup): Answer = {
    val answerParts = sag.answer.indices.map(i => getAnswerPart(i, sag))
    val dgroups = getDGroups(sag.answer, sag.derivations)
    val queryEntities = getNonAnswerEntities(sag.derivations)
    Answer(answerParts, dgroups, queryEntities)
  }

  def getDGroups(answerStrings: Seq[String], derivs: Seq[AnswerDerivation]): Seq[DerivationGroup] = {

    // group by execQuery
    val eqgroup = derivs.groupBy(_.execTuple.query)
    // break out get paraphrases for each group
    val eqgroupPPs = eqgroup.iterator.toSeq.map { case (equery, ds) => (equery, ds, ds.map(_.paraphrase).distinct.toSet) }
    // group by the paraphrase set
    val ppgroup = eqgroupPPs.groupBy(_._3)
    // convert to (paraphrases, uquery, derivs)
    val dgroups = ppgroup.iterator.toSeq.flatMap { case (pps, qderivspps) =>
      qderivspps.map { case (_, ds, _) =>
        // get the most common parsedQuery
        val qTriples = ds.flatMap(d => getTriples(answerStrings, d))
        val qGroups = qTriples.groupBy(_._1).iterator.toSeq
        val qConjTriples = qGroups.map { case (q, qtriples) => (q, qtriples.map(_._2)) }
        DerivationGroup(pps.toSeq, qConjTriples)
      }
    }
    dgroups
  }

  private val sourceIdRegex = """\w+\.source_ids""".r

  def getTriples(answerStrings: Seq[String], deriv: AnswerDerivation): Seq[(String, Triple)] = {

    val equery = deriv.execTuple.query
    val tuple = deriv.execTuple.tuple

    // get map from variable => answer string
    val answerMap = equery.qVars.zip(answerStrings).toMap
    // expand conjuncts to (c, c's qattrs)
    val conjunctsMap = equery.conjuncts.map { c => (c, equery.qAttrs.filter(_.startsWith(c.name))) }
    // expand to (c, c's attrs, triple)
    val triplesMap = conjunctsMap.map { case (conj, cAttrs) =>
      val sourceIdsOpt = tuple.get(conj.name + ".source_ids").map(_.asInstanceOf[List[String]])
      val triples = sourceIdsOpt match {
        case Some(sourceIds) =>
          getSolrDocs(sourceIds) map getOpenIETriple
        case None => Seq(getDefaultTriple(conj.name, tuple))
      }
      (conj, cAttrs, triples)
    }

    // convert to ConjunctString, Triple
    triplesMap.flatMap { case (conj, cAttrs, triples) =>
      val cstring = getConjunctString(conj, answerMap)
      triples map (t => (cstring, t))
    }
  }

  def getConjunctString(conj: TConjunct, qVarsToAnswers: Map[TVariable, String]): String = {
    def fieldString(f: Field) = conj.values(f) match {
      case v: TVariable => qVarsToAnswers(v)
      case l: TLiteral => literalToString(l)
    }

    Seq(arg1, rel, arg2).map(fieldString).mkString("(", ", ", ")")
  }

  def literalToString(l: TLiteral): String = l match {
      case UnquotedTLiteral(s) => s
      case QuotedTLiteral(s) => s
      case SetTLiteral(lits) => (lits map literalToString).mkString("(", "\" or \"", ")")
      case _ => l.toString
  }

  def getDefaultTriple(conjName: String, tuple: Tuple): Triple = {
    val arg1 = tuple.getString(conjName + ".arg1").get
    val rel = tuple.getString(conjName + ".arg1").get
    val arg2 = tuple.getString(conjName + ".arg1").get
    val source = tuple.getString(conjName + ".namespace").get
    val url = source.toLowerCase match {
      // TODO: Construct link to relevant source pages
      case "freebase" => "http://www.freebase.com/"
      case "nell" => "http://rtw.ml.cmu.edu/rtw/"
      case "probase" => "http://research.microsoft.com/en-us/projects/probase/"
      case _ => ""
    }
    DefaultTriple(arg1, rel, arg2, source, url)
  }

  def getSolrDocs(ids: Seq[String]): Seq[SolrDocument] = {

    // make the solr query
    val idQueryString = s"id:" + ids.mkString("(", " OR ", ")")
    val solrQuery = new SolrQuery(idQueryString)
    val response = solr.query(solrQuery)
    val docs = response.getResults() // SolrDocumentList is a poor implementation of a java collection, and breaks for comprehensions and javaconver(ters/sions)
    val scalaDocs = (0 until docs.size).map(docs.get)  // so get the elements out the hard way
    val foundIds = scalaDocs.map(_.getFieldValue("id").asInstanceOf[String]).toSet
    for (notFoundId <- ids.toSet &~ foundIds)
      Logger.warn(s"Solr returned no metadata doc for id:$notFoundId")
    scalaDocs
  }

  def getOpenIETriple(doc: SolrDocument): OpenIETriple = {

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
    OpenIETriple(tokens, arg1Interval, relInterval, arg2Interval, confidence, corpus, "Open IE", urlString)
  }

  val minContentConfidence = 0.5
  val maxRelLength = 60

  def filterResults(triple: Triple): Boolean = {
    triple match {
      case oie: OpenIETriple => oie.confidence >= minContentConfidence && oie.rel.length <= maxRelLength
      case _ => triple.rel.length <= maxRelLength
    }

  }
}

object AnswerConverter {

   import models.TypeFilters.enrichFreeBaseType

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
    val allAttrs = deriv.execTuple.tuple.attrs.keySet
    val answerAttrs = deriv.execTuple.query.qAttrs.toSet
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
      getLink(prefix, field, deriv.execTuple.tuple)
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
    val attrGroups = derivs.groupBy(_.execTuple.query.qAttrs(part))
    attrGroups.toSeq.flatMap { case (attr, ds) =>
      val tuples = ds.map(_.execTuple.tuple)
      tuples.flatMap { t =>
        getPrefixAndField(attr).flatMap { case (prefix, field) =>
          getLink(prefix, field, t)
        }
      }
    }
  }

  private val minEntityScore = 5.0

  private def filterLink(link: Link): Boolean = {
    link.entity.score > minEntityScore
  }

  private def getAnswerPart(part: Int, sag: ScoredAnswerGroup): AnswerPart = {
    // "lemma"
    val lemma = sag.answer(part)

    val attrs = sag.derivations.map(_.execTuple.query.qAttrs(part)).toSet

    val synonyms = sag.alternates.map(_(part))

    val links = getAllLinks(part, sag.derivations).filter(filterLink)

    val sortedLinks = links.groupBy(_.entity).toSeq.sortBy(-_._2.size)

    val topLink = sortedLinks.headOption.flatMap(_._2.headOption)

    val entity = topLink.map(_.entity)

    val fbTypes = topLink.map(_.types).getOrElse(Set.empty).toSeq.sortBy(-_.weight)

    // hack: add nell types at the start of the list
    val nellTypes = fbTypes.flatMap { fbt =>
      models.NellType.fbToNellType.get(fbt)
    }

    AnswerPart(lemma, attrs, synonyms, entity, nellTypes ++ fbTypes)
  }
}