package io.rout.example.xml


object Main extends App {
  import io.rout._
  import io.routs._
  import io.rout.xml._
  import cats._
  import implicits._

  val bodyXml = body.asXml[Payload]

  val xmlPayload = post(Root).sync(bodyXml)(payload => Ok(payload))

  serve(mkRoutes(Seq(xmlPayload)).service)

}

object Semiauto extends App {
  import io.rout._
  import io.routs._
  import cats._
  import implicits._
  import io.rout.generic.xml.semiauto._

   val decoder = decode[Payload]

  val encoder = encode[Payload]

  implicit val e = encoder.encodeXml

  implicit val d = decoder.decodeXml

  val bodyXml = body.asXml[Payload]

  val xmlPayload = post(Root).sync(bodyXml)(payload => Ok(payload))

  serve(mkRoutes(Seq(xmlPayload)).service)

}
