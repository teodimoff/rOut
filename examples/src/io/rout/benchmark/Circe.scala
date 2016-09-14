package io.rout.example.benchmark

import io.rout._
import io.routs._
import io.rout.circe._
import io.circe.generic.auto._

object Circe extends App {

  val payloadJson = post(Root).sync(binaryBody.asJson[Payload])(payload => Ok(payload))

  serve(mkRoutes(Seq(payloadJson)).service)
}
