package io.rout.benchmark

import io.rout._
import io.rout.circe._
import io.rout.generic.decoding._
import io.routs._
import io.circe.generic.auto._

object Benchmark extends App {

  val jsonPayload = binaryBody.asJson[Payload]

  val paramsPayload: ReqRead[Payload] = derive[Payload].fromParams

  val payloadParams = post(Root / "params")(paramsPayload) { payload =>
    Created(payload.toString)
  }

  val payloadParamsAuth = post(Root / "params" / "auth").filter[AuthedReq,Payload](paramsPayload) { (auth, payload) =>
    Created(payload.toString)
  }

  val payloadParamsPathAuth =
    post(Root / "params" / "auth" / Match[String]).filter[AuthedReq,Payload](paramsPayload) { (auth, string, payload) =>
      Created(payload.toString)
    }

  val payloadJsonAuth = post(Root / "json" / "auth").filter[AuthedReq,Payload](jsonPayload) { (auth, payload) =>
    Created(payload)
  }

  val payloadJsonPathAuth =
    post(Root / "json" / "auth" / Match[String]).filter[AuthedReq,Payload](jsonPayload) { (auth, string, payload) =>
    Created(payload)
  }

  val payloadJson = post(Root / "json")(jsonPayload)(p => Ok(p))

  val rOut = mkRoutes(Seq(
    payloadParams,
    payloadJson,
    AuthFilter.auth andThen payloadParamsAuth,
    AuthFilter.auth andThen payloadJsonAuth,
    AuthFilter.auth andThen payloadParamsPathAuth,
    AuthFilter.auth andThen payloadJsonPathAuth
  )).withNotFound("path was not found")

  serve(rOut.service)

}
