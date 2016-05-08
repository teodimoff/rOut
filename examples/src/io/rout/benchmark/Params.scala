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
