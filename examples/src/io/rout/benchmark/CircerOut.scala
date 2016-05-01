package io.rout.benchmark

import io.rout._
import io.routs._
import io.rout.circe._
import io.circe.generic.auto._


object CircerOut extends App {

  val body = binaryBody.asJson[Payload]

  val payloadAuth = post(Root / "auth").filter[AuthedReq,Payload](body) { (auth, payload) =>
    Created(payload)
  }

  val payload = post(Root)(body)(p => Ok(p))

  serve(mkRoutes(Seq(
    payload,
    AuthFilter.auth andThen payloadAuth
  )).service)

}
