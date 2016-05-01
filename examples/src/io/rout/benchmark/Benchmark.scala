package io.rout.benchmark

import io.rout._
import io.rout.circe._
import io.rout.generic.decoding._
import io.routs._
import io.circe.generic.auto._

object Benchmark extends App {

  val body = binaryBody.asJson[Payload]

  val passportDerive = derive[Passport].fromParams

  val derivedPayload: ReqRead[Payload] = derive[Payload].fromParams

  val payloadParams = post(Root / "params")(derivedPayload) { payload =>
    Created(payload.toString)
  }

  val payloadParamsAuth = post(Root / "params" / "auth").filter[AuthedReq,Payload](derivedPayload) { (auth, payload) =>
    Created(payload.toString)
  }

  val payloadParamsPathAuth =
    post(Root / "params" / "auth" / Match[String]).filter[AuthedReq,Payload](derivedPayload) { (auth, string, payload) =>
      Created(payload.toString)
    }

  val payloadJsonAuth = post(Root / "json" / "auth").filter[AuthedReq,Payload](body) { (auth, payload) =>
    Created(payload)
  }

  val payloadJsonPathAuth =
    post(Root / "json" / "auth" / Match[String]).filter[AuthedReq,Payload](body) { (auth, string, payload) =>
    Created(payload.toString)
  }

  val payloadJson = post(Root / "json")(body)(p => Ok(p))

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
