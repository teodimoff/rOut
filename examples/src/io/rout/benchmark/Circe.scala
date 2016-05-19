package io.rout.example.benchmark

import io.rout._
import io.routs._
import io.rout.circe._
import io.circe.generic.auto._

object Circe extends App {

  val bodyPayload = binaryBody.asJson[Payload]

  val payloadJson = post(Root)(bodyPayload)(payload => Ok(payload))

  serve(mkRoutes(Seq(payloadJson)).service)
}
