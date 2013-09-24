package models

import sjson.json.DefaultProtocol
import sjson.json.Format

@SerialVersionUID(4021L) // last: 4011
case class FreeBaseEntity(val name: String, val fbid: String, val score: Double, val inlinkRatio: Double) {
  override def toString = "FreeBaseEntity" + Iterable(name, fbid, "%1.2f" format score, inlinkRatio).mkString("(", ", ", ")")
}

object FreeBaseEntityProtocol extends DefaultProtocol {
  implicit val ExtractionEntityFormat: Format[FreeBaseEntity] =
    asProduct4("name", "fbid", "score", "inlink")(FreeBaseEntity.apply)(FreeBaseEntity.unapply(_).get)
}

@SerialVersionUID(4012L)
case class FreeBaseType(val domain: String, val typ: String) {
  require(!domain.isEmpty)
  require(!typ.isEmpty)

  def name = "/"+domain+"/"+typ

  override def toString: String = "FreeBaseType(%s, %s)".format(domain, typ)
}

object FreeBaseType {
  def parse(string: String): Option[FreeBaseType] = {
    string.dropWhile(ch=>ch == '_' || ch == '/').split("/").toList match {
      case domain :: typ => {
        if (typ.isEmpty) return None
        if (typ.head.isEmpty) return None
        if (domain.isEmpty) return None
        else Some(FreeBaseType(domain.toLowerCase, typ.mkString("/").toLowerCase))
      }
      case _ => None
    }
  }
}

object FreeBaseTypeProtocol extends DefaultProtocol {
  implicit val FreeBaseTypeFormat: Format[FreeBaseType] =
    asProduct2("domain", "type")(FreeBaseType.apply)(FreeBaseType.unapply(_).get)
}