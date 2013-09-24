package models

import scala.collection.Seq
import scala.io.Source
import edu.knowitall.common.Resource.using
import edu.knowitall.common.Timing
import org.slf4j.Logger
import org.slf4j.LoggerFactory

object NellType {

  val logger = LoggerFactory.getLogger(this.getClass)

  val nellToFbTypeMapFile = "/nelltypes-to-fbtypes.txt"
  val nellToFbTypeMapUrl = this.getClass.getResource(nellToFbTypeMapFile)
  require(nellToFbTypeMapUrl != null, "Could not find resource: " + nellToFbTypeMapFile)

  // make this a map to fbTypes
  lazy val fbToNellType: Map[FreeBaseType, NellType] = {
    val (nsLoad, resultMap) = Timing.time {
      using(Source.fromInputStream(nellToFbTypeMapUrl.openStream)) { source =>
        source.getLines.flatMap { line =>
          line.split("\t") match {
            case Array(nellString, fbString, nameString, _*) => FreeBaseType.parse(fbString) match {
              case Some(fbType) => Some((fbType, NellType(nellString, nameString)))
              case None => "nellToFbType fbType string parse error: %s".format(fbString); None
            }
            case _ => System.err.println("nellToFbType parse error: %s".format(line)); None
          }
        }.toMap
      }
    }
    logger.info("Loaded FreeBase type to NELL type dictionary in %s".format(Timing.Milliseconds.format(nsLoad)))
    resultMap
  }
}

case class NellType(val id: String, val name: String)