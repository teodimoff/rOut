package io.rout.example.benchmark

import io.rout._
import io.routs._
import io.rout.generic.decoding._

object ParamsAuth extends App {

  val derivedPayload: ReqRead[Payload] = derive[Payload].fromParams

  val payload = post(Root).filter[AuthedReq].sync(derivedPayload) { (auth, payload) =>
    Created(payload.toString)
  }

  val rOut = mkRoutes(Seq(
    AuthFilter.auth andThen payload
  ))

  serve(rOut.service)
}
