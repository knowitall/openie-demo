package controllers

import edu.knowitall.scoring.ScoredAnswerGroup
import edu.knowitall.apps.QASystem
import edu.knowitall.execution.AnswerDerivation

import models.Answer
import models.AnswerTitle
import models.AnswerTitlePart
import models.ExtractionPart
import models.Argument1
import models.Relation
import models.Argument2
import models.Query
import models.FreeBaseEntity

import edu.knowitall.apps.QASystem


/**
 * Takes a model Query and executes it against a QASystem.
 */
object QAExecutor {

  // a representation of the result set
  abstract class Result[T]
  case class Success[T](groups: Seq[T]) extends Result[T]
  case class Timeout[T](groups: Seq[T]) extends Result[T]
  case class Limited[T](groups: Seq[T]) extends Result[T]

  val qaSystem : QASystem = QASystem.getInstance().get

  def execute(query: Query) : Result[Answer] = {
    val qaQuery = transformQuery(query)
    val answers = qaSystem.answer(qaQuery)
    transformAnswerGroup(answers)
  }

  /**
   * Turns the given model.Query into a String to serve as input to the QASystem.
   */
  private def transformQuery(query: Query) : String = {
    
    // QASystem takes comma-separated plain text.
    query.arg1StringField.getOrElse("?") + ", " +
    query.relStringField.getOrElse("?") + ", " +
    query.arg2StringField.getOrElse("?")
  }

  /**
   * Transforms a List[ScoredAnswerGroup] returned from the QASystem into a 
   * model Result[Answer] for the frontend to use.
   */
  private def transformAnswerGroup(answers: List[ScoredAnswerGroup]) : Result[Answer] = { 

    val resultAnswers = for(sag <- answers) yield answerFromDerivations(sag.derivations)

    Success(resultAnswers)
  }

  /**
   * Convert the given list of AnswerDerivations to a model Answer.  Right now it looks at only the
   * first answer-tuple returned.
   */
  private def answerFromDerivations(derivations: List[AnswerDerivation]) : Answer = {

    // want to get the tuples out of the answer derivations
    // this is kludged from triplestore-qa JsonSerialization.ExecQueryInv.fromDerivs
    val grpd = derivations.groupBy(_.etuple.equery)
    val tuples = for ((eqr, dsg) <- grpd; ts = dsg.map(_.etuple.tuple)) yield ts.map(_.attrs)

    val tuple = tuples.head.head; // take the first result from the first derivation

    val arg1Entity: Option[FreeBaseEntity] = tuple.get("r0.arg1_entity_id") match {
      case Some(id) => Some(FreeBaseEntity(tuple.get("r0.arg1_entity_name").get.asInstanceOf[String],
                                      id.asInstanceOf[String],
                                      tuple.get("r0.arg1_entity_score").get.asInstanceOf[Double],
                                      tuple.get("r0.arg1_entity_inlink_ratio").get.asInstanceOf[Double]))
      case None => None
    }

    val arg2Entity: Option[FreeBaseEntity] = tuple.get("r0.arg2_entity_id") match {
      case Some(id) => Some(FreeBaseEntity(tuple.get("r0.arg2_entity_name").get.asInstanceOf[String],
                                      id.asInstanceOf[String],
                                      tuple.get("r0.arg2_entity_score").get.asInstanceOf[Double],
                                      tuple.get("r0.arg2_entity_inlink_ratio").get.asInstanceOf[Double]))
      case None => None
    }

    val arg1 = AnswerTitlePart(tuple.get("r0.arg1").get.asInstanceOf[String], // should theoretically be a lemma
                               Argument1,
                               Seq(), // TODO could take synonyms from subsequent tuples
                               arg1Entity, 
                               Set())

    val rel = AnswerTitlePart(tuple.get("r0.rel").get.asInstanceOf[String],
                              Relation,
                              Seq(),
                              None,
                              Set())

    val arg2 = AnswerTitlePart(tuple.get("r0.arg2").get.asInstanceOf[String], 
                               Argument2, 
                               Seq(),
                               arg2Entity,
                               Set())


    val title = AnswerTitle(" ", Seq(arg1, rel, arg2))

    Answer(title, 
           List(), // QASystem doesn't give back Content (sentences)
           List()) // Not sure what Answer.queryEntity is supposed to contain
  }
}

