package io.rout.benchmark

import io.rout._
import io.routs._
import io.rout.circe._
import io.circe.generic.auto._

object CirceAuth extends App {

  val body = binaryBody.asJson[Payload]

  val payloadAuth = post(Root).filter[AuthedReq](body) { (auth, payload) =>
    Created(payload)
  }

  serve(mkRoutes(Seq(
    AuthFilter.auth andThen payloadAuth
  )).service)

}
