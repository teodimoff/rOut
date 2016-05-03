package io.rout.benchmark

import io.rout._
import io.routs._
import io.rout.circe._
import io.circe.generic.auto._

object Circe extends App {

  val body = binaryBody.asJson[Payload]
  
  val payloadJson = post(Root)(body)(payload => Ok(payload))

  serve(mkRoutes(Seq(payloadJson)).service)

}
