package io.rout

import shapeless.Witness

object contentTypes {

  /*
  todo: make content-types bound by toplevel ContentType not by string and def for other content-types

    type ContentType <: String

    def withType(ct: String): Witness.Aux[String] =Witness.mkWitness(ct)
   */

  object Application {
    type Json  = Witness.`"application/json"`.T
    type Xml = Witness.`"application/xml"`.T
    type AtomXml = Witness.`"application/atom+xml"`.T
    type Csv = Witness.`"application/csv"`.T
    type Javascript = Witness.`"application/javascript"`.T
    type OctetStream = Witness.`"application/octet-stream"`.T
    type RssXml = Witness.`"application/rss+xml"`.T
    type WwwFormUrlencoded = Witness.`"application/x-www-form-urlencoded"`.T

  }

  object Text {
    type Plain = Witness.`"text/plain"`.T
    type Html = Witness.`"text/html"`.T
  }


}

/**
  * Representations for the various types that can be processed with [[ReqRead]]s.
  */
object items {
  sealed abstract class RequestItem(val kind: String, val nameOption:Option[String] = None) {
    val description = kind + nameOption.fold("")(" '" + _ + "'")
  }
  final case class ParamItem(name: String) extends RequestItem("param", Some(name))
  final case class HeaderItem(name: String) extends RequestItem("header", Some(name))
  final case class CookieItem(name: String) extends RequestItem("cookie", Some(name))
  case object BodyItem extends RequestItem("body")
  case object MultipleItems extends RequestItem("request")
}
