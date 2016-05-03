package io.rout.benchmark

import io.rout._
import io.routs._
import io.rout.circe._
import io.circe.generic.auto._

object Circe extends App {

  val body = binaryBody.asJson[Payload]
  
  val payloadJson = post(Root)(body)(payload => Ok(payload))

  serve(mkRoutes(Seq(
    payloadJson
  )).service)

}

object CirceAuth extends App {

  val body = binaryBody.asJson[Payload]

  val payloadAuth = post(Root / "auth").filter[AuthedReq,Payload](body) { (auth, payload) =>
    Created(payload)
  }

  serve(mkRoutes(Seq(
    AuthFilter.auth andThen payloadAuth
  )).service)

}
