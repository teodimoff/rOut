package io.rout.benchmark

import io.rout._
import io.routs._
import io.rout.generic.decoding._

object Params extends App {

  val derivedPayload: ReqRead[Payload] = derive[Payload].fromParams

  val regularPayload = post(Root)(derivedPayload) { payload =>
    Created(payload.toString)
  }

  val rOut = mkRoutes(Seq(
    regularPayload
  ))

  serve(rOut.service)
}

object ParamsAuth extends App {

  val derivedPayload: ReqRead[Payload] = derive[Payload].fromParams

  val payload = post(Root / "auth").filter[AuthedReq](derivedPayload) { (auth, payload) =>
    Created(payload.toString)
  }

  val rOut = mkRoutes(Seq(
    AuthFilter.auth andThen payload
  ))

  serve(rOut.service)
}
