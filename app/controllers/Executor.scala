package controllers

import java.util.regex.Pattern
import java.io.ByteArrayInputStream
import java.io.ObjectInputStream
import edu.washington.cs.knowitall.browser.extraction.Extraction
import edu.washington.cs.knowitall.browser.extraction.ExtractionArgument
import edu.washington.cs.knowitall.browser.extraction.ExtractionRelation
import edu.washington.cs.knowitall.browser.extraction.ExtractionGroup
import edu.washington.cs.knowitall.browser.extraction.FreeBaseEntity
import edu.washington.cs.knowitall.browser.extraction.FreeBaseType
import edu.washington.cs.knowitall.browser.extraction.Instance
import edu.washington.cs.knowitall.browser.extraction.InstanceDeduplicator
import edu.washington.cs.knowitall.browser.extraction.ReVerbExtraction
import edu.washington.cs.knowitall.browser.extraction.ReVerbExtractionGroup
import edu.washington.cs.knowitall.browser.lucene
import edu.washington.cs.knowitall.browser.lucene.QuerySpec
import edu.washington.cs.knowitall.common.Timing
import edu.washington.cs.knowitall.common.Resource.using
import edu.washington.cs.knowitall.tool.postag.PostaggedToken
import edu.washington.cs.knowitall.tool.postag.Postagger
import edu.washington.cs.knowitall.tool.stem.MorphaStemmer
import edu.washington.cs.knowitall.tool.tokenize.OpenNlpTokenizer
import models.Answer
import models.AnswerTitle
import models.AnswerTitlePart
import models.Argument1
import models.Argument2
import models.Query
import models.Query.Constraint
import models.Query.CorporaConstraint
import models.Query.EntityConstraint
import models.Query.Fixed
import models.Query.TermConstraint
import models.Query.TypeConstraint
import models.Relation
import models.TypeFilters
import models.TypeFilters.enrichFreeBaseType
import play.api.Logger
import akka.actor.TypedActor
import play.libs.Akka
import akka.actor.TypedProps
import edu.washington.cs.knowitall.browser.lucene.LuceneFetcher
import edu.washington.cs.knowitall.browser.lucene.ResultSet
import org.apache.solr.client.solrj.impl.HttpSolrServer
import org.apache.solr.client.solrj.SolrQuery
import java.io.FileInputStream

object Executor {
  type REG = ExtractionGroup[ReVerbExtraction]
  
  val tokenizer = {
    Timing.timeThen {
      new OpenNlpTokenizer()
    } { ns => Logger.info("Initialized OpenNlpTokenizer (" + Timing.Seconds.format(ns) + ")") }
  }

  sealed abstract class FetchSource
  case object LuceneSource extends FetchSource {
    lazy val fetcher = new lucene.ParallelExtractionGroupFetcher(
      paths,
      /* max search groups (20k)  */
      20000,
      /* max read instances (10k) */
      10000,
      /* timout in millis (10s) */
      10000)
  }
  case object ActorSource extends FetchSource {
    lazy val fetcher = TypedActor(Akka.system).typedActorOf(TypedProps[LuceneFetcher](), Akka.system.actorFor("akka://openie-lucene-server@reliable.cs.washington.edu:9002/user/fetcher"))
  }
  case object SolrSource extends FetchSource {
    val solr = new HttpSolrServer("http://reliable.cs.washington.edu:8983/solr")
    solr.setSoTimeout(20000); // socket read timeout
    solr.setConnectionTimeout(20000);
    solr.setDefaultMaxConnectionsPerHost(100);
    solr.setMaxTotalConnections(100);
    solr.setFollowRedirects(false); // defaults to false
    solr.setAllowCompression(true);
    solr.setMaxRetries(1); // defaults to 0.  > 1 not recommended.

    def fetch(spec: QuerySpec) = {
      val squery = new SolrQuery()
      
      def normalize(string: String) = {
        val tokenized = tokenizer.synchronized {
          tokenizer.tokenize(string)
        }
        
        (tokenized.map(_.string) map MorphaStemmer.instance.lemmatize).mkString(" ")
      }
      
      val parts = (Iterable("arg1", "rel", "arg2", "arg1_types", "arg2_types", "arg1_entity", "arg2_entity") zip Iterable(spec.arg1 map normalize, spec.rel map normalize, spec.arg2 map normalize, spec.arg1Types, spec.arg2Types, spec.arg1Entity, spec.arg2Entity)).flatMap { case(a, b) => if (b.isEmpty) {
          None
        }
        else {
          Some("+" + a, b.get)
        }}
      val queryText = parts.map{case (field, value) => field + ":\"" + value + "\""}.mkString(" ")
      println(queryText)
      squery.setQuery(queryText)
      squery.setSortField("size", SolrQuery.ORDER.desc)
      squery.setRows(1000)
      squery.setTimeAllowed(10000)
      val response = try {
        Timing.timeThen {
          solr.query(squery)
        } { ns =>
          Logger.debug("solr response received (" + Timing.Seconds.format(ns) + ")")
        }
      }
      import scala.collection.JavaConverters._

      val groups = for (result <- response.getResults().iterator().asScala) yield {
        val instances = using(new ByteArrayInputStream(result.getFieldValue("instances").asInstanceOf[Array[Byte]])) { is =>
          using(new ObjectInputStream(is) {
            override def resolveClass(desc: java.io.ObjectStreamClass): Class[_] = {
              try { Class.forName(desc.getName, false, getClass.getClassLoader) }
              catch { case ex: ClassNotFoundException => super.resolveClass(desc) }
            }
          }) { ois =>
            ois.readObject().asInstanceOf[List[Instance[ReVerbExtraction]]]
          }
        }

        val arg1 = ExtractionArgument(
          norm = result.getFieldValue("arg1_norm").asInstanceOf[String],
          entity = {
            if (!result.containsKey("arg1_entity_id")) None
            else {
              val id = result.getFieldValue("arg1_entity_id").asInstanceOf[String]
              val name = result.getFieldValue("arg1_entity_name").asInstanceOf[String]
              val inlink_ratio = result.getFieldValue("arg1_entity_inlink_ratio").asInstanceOf[Double]
              val score = result.getFieldValue("arg1_entity_score").asInstanceOf[Double]
              Some(FreeBaseEntity(name, id, score, inlink_ratio))
            }
          },
          types =
            if (!result.containsKey("arg1_fulltypes")) Set.empty
            else result.getFieldValue("arg1_fulltypes").asInstanceOf[java.util.List[String]].asScala.map(FreeBaseType.parse(_).get).toSet)

        val arg2 = ExtractionArgument(
          norm = result.getFieldValue("arg2_norm").asInstanceOf[String],
          entity = {
            if (!result.containsKey("arg2_entity_id")) None
            else {
              val id = result.getFieldValue("arg2_entity_id").asInstanceOf[String]
              val name = result.getFieldValue("arg2_entity_name").asInstanceOf[String]
              val inlink_ratio = result.getFieldValue("arg2_entity_inlink_ratio").asInstanceOf[Double]
              val score = result.getFieldValue("arg2_entity_score").asInstanceOf[Double]
              Some(FreeBaseEntity(name, id, score, inlink_ratio))
            }
          },
          types =
            if (!result.containsKey("arg2_fulltypes")) Set.empty
            else result.getFieldValue("arg2_fulltypes").asInstanceOf[java.util.List[String]].asScala.map(FreeBaseType.parse(_).get).toSet)

        val rel = ExtractionRelation(result.getFieldValue("rel_norm").asInstanceOf[String])

        ExtractionGroup[ReVerbExtraction](
          arg1 = arg1,
          rel  = rel,
          arg2 = arg2,
          instances = instances.toSet
        )
      }

      val (ns, list) = Timing.time {
        groups.toList
      }

      Logger.debug("groups retrieved: " + list.size + " (" + Timing.Seconds.format(ns) + ")")
      lucene.Success(list)
    }
  }

  final val SOURCE: FetchSource = SolrSource

  final val CONFIDENCE_THRESHOLD: Double = 0.5
  final val ENTITY_SCORE_THRESHOLD: Double = 5.0
  final val MAX_ANSWER_LENGTH = 60

  abstract class Result
  case class Success(groups: Seq[Answer]) extends Result
  case class Timeout(groups: Seq[Answer], hitCount: Int) extends Result
  case class Limited(groups: Seq[Answer], hitCount: Int) extends Result

  val paths = Seq("/scratch/common/openie-demo/index-1.0.4",
    "/scratch2/common/openie-demo/index-1.0.4",
    "/scratch3/common/openie-demo/index-1.0.4",
    "/scratch4/common/openie-demo/index-1.0.4")

  def fetch(querySpec: QuerySpec): ResultSet = {
    SOURCE match {
      case LuceneSource => LuceneSource.fetcher.getGroups(querySpec)
      case ActorSource => ActorSource.fetcher.fetch(querySpec)
      case SolrSource => SolrSource.fetch(querySpec)
    }
  }

  def execute(query: Query): Result = {
    def group: REG => AnswerTitle = {
      def part(eg: REG, part: Symbol) = {
        part match {
          case 'rel => AnswerTitlePart(eg.rel.norm, Relation, eg.instances.iterator.map(_.extraction.relText).map(Query.clean).toSeq, None, Set.empty)
          case 'arg1 => AnswerTitlePart(eg.arg1.norm, Argument1, eg.instances.iterator.map(_.extraction.arg1Text).map(Query.clean).toSeq, eg.arg1.entity, eg.arg1.types.toSet)
          case 'arg2 => AnswerTitlePart(eg.arg2.norm, Argument2, eg.instances.iterator.map(_.extraction.arg2Text).map(Query.clean).toSeq, eg.arg2.entity, eg.arg2.types.toSet)
        }
      }
      (query.arg1, query.rel, query.arg2) match {
        case (Some(_: Fixed), Some(_: Fixed), Some(_: Fixed)) => (eg: REG) =>
          AnswerTitle(" ", Seq(part(eg, 'arg1), part(eg, 'rel), part(eg, 'arg2)))

        case (Some(_: Fixed), Some(_: Fixed), _) => (eg: REG) => AnswerTitle("", Seq(part(eg, 'arg2)))
        case (_, Some(_: Fixed), Some(_: Fixed)) => (eg: REG) => AnswerTitle("", Seq(part(eg, 'arg1)))
        case (Some(_: Fixed), _, Some(_: Fixed)) => (eg: REG) => AnswerTitle("", Seq(part(eg, 'rel)))

        case (Some(_: Fixed), _, _) => (eg: REG) => AnswerTitle(" ", Seq(part(eg, 'rel), part(eg, 'arg2)))
        case (_, Some(_: Fixed), _) => (eg: REG) => AnswerTitle(", ", Seq(part(eg, 'arg1), part(eg, 'arg2)))
        case (_, _, Some(_: Fixed)) => (eg: REG) => AnswerTitle(", ", Seq(part(eg, 'arg1), part(eg, 'rel)))

        case _ => (eg: REG) => AnswerTitle(" ", Seq(part(eg, 'arg1), part(eg, 'rel), part(eg, 'arg2)))
      }
    }

    val (result, converted) = executeHelper(query)

    val (nsGroups, groups) = Timing.time { Answer.fromExtractionGroups(converted.toList, group, query.fullParts).filter(!_.title.text.trim.isEmpty) }

    Logger.debug("Converted to %d answers in %s".format(groups.size, Timing.Seconds.format(nsGroups)))

    result match {
      case lucene.Success(results) => Success(groups)
      case lucene.Limited(results, hitCount) => Limited(groups, hitCount)
      case lucene.Timeout(results, hitCount) => Timeout(groups, hitCount)
    }
  }

  def executeRaw(query: Query): List[ExtractionGroup[ReVerbExtraction]] = executeHelper(query)._2.sortBy(-_.instances.size)

  private def executeHelper(query: Query): (lucene.ResultSet, List[ExtractionGroup[ReVerbExtraction]]) = {

    def filterInstances(inst: Instance[ReVerbExtraction]): Boolean = {
      def clean(arg: String) = {
        var clean = Query.clean(arg.trim)

        clean = leadingArticle.matcher(clean).replaceAll("");

        clean.toLowerCase
      }

      def tooShort(part: String) = {
        part.size - nonQuestionableChars.matcher(part).replaceAll("").size <= 1
      }

      val relTokens = inst.extraction.sentenceTokens.slice(inst.extraction.relInterval.start, inst.extraction.relInterval.end)
      val arg2Tokens = inst.extraction.sentenceTokens.slice(inst.extraction.arg2Interval.start, inst.extraction.arg2Interval.end)

      val arg1clean = clean(inst.extraction.arg1Text)
      val arg2clean = clean(inst.extraction.arg2Text)
      val relclean = clean(inst.extraction.relText)
      val extr = arg1clean + relclean + arg2clean

      def negative = {
        val negatives = Set("no", "not", "none", "n't", "never")
        relTokens.exists(token =>
          negatives.contains(token.string.toLowerCase)) ||
          arg2Tokens.exists(token =>
            negatives.contains(token.string.toLowerCase))
      }

      def tooLong =
        inst.extraction.arg1Text.length + inst.extraction.arg2Text.length + inst.extraction.relText.length > MAX_ANSWER_LENGTH

      def containsPronoun =
        pronouns.contains(arg1clean) || pronouns.contains(arg2clean)

      def likelyError =
        likelyErrorPattern.matcher(arg1clean).matches() ||
          likelyErrorPattern.matcher(arg2clean).matches()

      if (negative ||
        tooLong ||
        containsPronoun ||
        inst.confidence < CONFIDENCE_THRESHOLD ||
        (arg1clean.isEmpty || relclean.isEmpty || arg2clean.isEmpty) ||
        (arg1clean == arg2clean) ||
        (nonQuestionableChars.matcher(extr).replaceAll("").size >= 5) ||
        (tooShort(arg1clean) || tooShort(relclean) || tooShort(arg2clean)) ||
        likelyError) {
        false
      } else {
        true
      }
    }



    def filterCorpora(instance: Instance[_ <: Extraction]) = query.corpora match {
      case Some(CorporaConstraint(corporaString)) => corporaString.contains(instance.corpus)
      case _ => true
    }

    def filterRelation(relNorm: Option[String])(group: ExtractionGroup[ReVerbExtraction]) = relNorm match {
      // if the query does not constrain rel, we can ignore this filter
      case Some(queryRelNorm) => {
        val filteredRelNormTokens = (queryRelNorm.toLowerCase.split(" ").filter { str => !(Postagger.prepositions contains str) }).toSet
        if (!filteredRelNormTokens.isEmpty) filterRelationHelper(filteredRelNormTokens, group) else true
      }
      case None => true
    }

    def filterRelationHelper(filteredRelNormTokens: Set[String], group: ExtractionGroup[ReVerbExtraction]): Boolean = {
      group.instances.headOption match {
        // it's possible that the group is empty already due to some other filter.
        // If it is, ignore (a different filter checks for this)
        case Some(group) => {
          val extr = group.extraction
          def filterNonContent(tok: PostaggedToken): Boolean = nonContentTag.findFirstIn(tok.postag).isEmpty
          val groupRelNormTokens = extr.normTokens(extr.relInterval) filter filterNonContent
          val lastContentWord = groupRelNormTokens.last.string
          filteredRelNormTokens.contains(lastContentWord)
        }
        case None => true
      }
    }



    def filterArg2DayOfWeek(group: ExtractionGroup[_ <: Extraction]): Boolean = {
      !daysOfWeek.contains(group.arg2.norm)
    }

    def filterGroups(spec: QuerySpec)(group: ExtractionGroup[_ <: Extraction]): Boolean = {
      // if there are constraints, apply them to each part in case lucene messed up
      def filterPart(constraint: Option[Constraint], entity: Option[FreeBaseEntity], types: Iterable[FreeBaseType]) = {
        constraint.map {
          _ match {
            case TypeConstraint(typ) => types.exists(_.typ equalsIgnoreCase typ)
            case EntityConstraint(ent) => entity.exists(_.name equalsIgnoreCase ent)
            case _ => true
          }
        }.getOrElse(true)
      }

      if (group.arg1.norm.trim.isEmpty || group.rel.norm.trim.isEmpty || group.arg2.norm.trim.isEmpty) {
        // we have an empty part
        false
      } else {
        // make sure constraints pass
        filterPart(query.arg1, group.arg1.entity, group.arg1.types) && filterPart(query.arg2, group.arg2.entity, group.arg2.types)
      }
    }

    def queryTerms(constraint: Option[Query.Constraint]): Option[String] = constraint match {
      case Some(TermConstraint(term)) => Some(term)
      case _ => None
    }

    def queryTypes(constraint: Option[Query.Constraint]): Option[String] = constraint match {
      case Some(TypeConstraint(typ)) => Some(typ)
      case _ => None
    }

    def queryEntity(constraint: Option[Query.Constraint]): Option[String] = constraint match {
      case Some(EntityConstraint(entity)) => Some(entity)
      case _ => None
    }

    def queryCorpora(constraint: Option[Query.CorporaConstraint]): Option[String] = constraint match {
      case Some(CorporaConstraint(corpString)) => Some(corpString)
      case _ => None
    }

    // execute the query
    val spec = QuerySpec(queryTerms(query.arg1), queryTerms(query.rel), queryTerms(query.arg2), queryEntity(query.arg1), queryEntity(query.arg2), queryTypes(query.arg1), queryTypes(query.arg2), queryCorpora(query.corpora))
    val (nsQuery, result) = Timing.time {
      fetch(spec)
    }

    // open up the retrieved case class
    val (results, hitCount) = result match {
      case lucene.Success(results) => (results, results.size)
      case lucene.Limited(results, hitCount) => (results, hitCount)
      case lucene.Timeout(results, hitCount) => (results, hitCount)
    }
    Logger.debug(spec.toString + " searched with " + results.size + " groups (" + result.getClass.getSimpleName + ") in " + Timing.Seconds.format(nsQuery))

    val (nsRegroup, regrouped) = Timing.time {
      ReVerbExtractionGroup.indexGroupingToFrontendGrouping(results)
    }
    Logger.debug(spec.toString + " regrouped with " + regrouped.size + " answers (" + result.getClass.getSimpleName + ") in " + Timing.Seconds.format(nsRegroup))

    // apply backend deduplication
    // TODO: merge into index itself
    val (nsDeduped, deduped) = Timing.time {
      regrouped map InstanceDeduplicator.deduplicate
    }
    Logger.debug(spec.toString + " deduped with " + deduped.size + " answers (" + result.getClass.getSimpleName + ") in " + Timing.Seconds.format(nsDeduped))

    def entityFilter(entity: FreeBaseEntity) =
      entity.score > ENTITY_SCORE_THRESHOLD

    def typeFilter(typ: FreeBaseType) = {
      import TypeFilters._
      typ.valid
    }

    val (nsFiltered, filtered: List[ExtractionGroup[ReVerbExtraction]]) =
      Timing.time {
        deduped.iterator.map { reg =>
          // normalize fields and remove filtered entities/types
          val arg1Entity = reg.arg1.entity filter entityFilter
          val arg1EntityRemoved = reg.arg1.entity.isDefined && arg1Entity.isEmpty
          val arg1Types = if (!arg1EntityRemoved) reg.arg1.types filter typeFilter else Set.empty[FreeBaseType]

          val arg2Entity = reg.arg2.entity filter entityFilter
          val arg2EntityRemoved = reg.arg2.entity.isDefined && arg2Entity.isEmpty
          val arg2Types = if (!arg2EntityRemoved) reg.arg2.types filter typeFilter else Set.empty[FreeBaseType]

          reg.copy(
            instances = reg.instances,// filter filterCorpora filter filterInstances,
            arg1 = ExtractionArgument(Query.clean(reg.arg1.norm), arg1Entity, arg1Types),
            rel = reg.rel.copy(norm = Query.clean(reg.rel.norm)),
            arg2 = ExtractionArgument(Query.clean(reg.arg2.norm), arg2Entity, arg2Types))
        }.toList filter filterGroups(spec) filter filterRelation(spec.relNorm) filter (_.instances.size > 0) filter filterArg2DayOfWeek toList
      }

    Logger.debug(spec.toString + " filtered with " + filtered.size + " answers in " + Timing.Seconds.format(nsFiltered))

    (result, filtered)
  }

  private val nonContentTag = "IN|TO|RB?".r
  private final val pronouns: Set[String] = Set("he", "she", "they", "them",
    "that", "this", "who", "whom", "i", "you", "him", "her", "we",
    "it", "the", "a", "an")
  private final val nonQuestionableChars = Pattern.compile("[\\p{Lower}\\p{Digit} ]+")
  private final val leadingArticle = Pattern.compile("^\\s*(the|this|these|those|that|a|an)\\s*", Pattern.CASE_INSENSITIVE)
  private final val startCap = Pattern.compile(".*\\b[A-Z].*")
  private final val likelyErrorPattern = Pattern.compile(".*(http|\\(|\\)|\\\"|\\[|thing).*", Pattern.CASE_INSENSITIVE)

  private final val daysOfWeek = Set("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday", "mon.", "tues.", "wed.", "thurs.", "fri.", "sat.", "sun.", "thurs", "tues", "mon", "last week")

}
