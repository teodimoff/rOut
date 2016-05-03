package io.rout.benchmark

import io.rout._
import io.routs._
import io.rout.generic.decoding._

object Rout extends App {

  val derivedPayload: ReqRead[Payload] = derive[Payload].fromParams

  val regularPayload = post(Root)(derivedPayload) { payload =>
    Created(payload.toString)
  }

  val rOut = mkRoutes(Seq(
    regularPayload
  )).withNotFound("path was not found")

  serve(rOut.service)
}

object RoutAuth extends App {

  val derivedPayload: ReqRead[Payload] = derive[Payload].fromParams

  val payload = post(Root / "auth").filter[AuthedReq,Payload](derivedPayload) { (auth, payload) =>
    Created(payload.toString)
  }

  val rOut = mkRoutes(Seq(
    AuthFilter.auth andThen payload
  )).withNotFound("path was not found")

  serve(rOut.service)
}
