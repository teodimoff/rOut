package io.rout.benchmark

import io.rout._
import io.routs._
import io.rout.generic.decoding._

object Rout extends App {

  val passportDerive = derive[Passport].fromParams

  val derivedPayload: ReqRead[Payload] = derive[Payload].fromParams

  val payload = post(Root / "auth").filter[AuthedReq,Payload](derivedPayload) { (auth, payload) =>
    Created(payload.toString)
  }

  val payloadPath =
    post(Root / "auth" / Match[String]).filter[AuthedReq,Payload](derivedPayload) { (auth, string, payload) =>
    Created(payload.toString)
  }

  val regularPayload = post(Root)(derivedPayload) { payload =>
    Created(payload.toString)
  }

  val rOut = mkRoutes(Seq(
    regularPayload,
    AuthFilter.auth andThen payload,
    AuthFilter.auth andThen payloadPath
  )).withNotFound("path was not found")

  serve(rOut.service)
}
