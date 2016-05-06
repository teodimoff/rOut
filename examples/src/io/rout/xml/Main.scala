package io.rout.example.xml

import io.rout._
import io.routs._
import io.rout.xml._
import cats._
import implicits._

object Main extends App {

  val bodyXml = body.asXml[Payload]

  val xmlPayload = post(Root)(bodyXml)(payload => Ok(payload))

  serve(mkRoutes(Seq(xmlPayload)).service)

}

/*
import cats.data.Xor
import com.twitter.util.Try
import io.rout._
import io.routs._
import cats._
import implicits._
import io.rout.generic.xml.semiauto._

import io.rout.example.xml._

object Main extends App {

   val decode = derive[Payload].xmlDecoder

   val encode = derive[Payload].xmlEncoder

  implicit val cc: Decode.ApplicationXml[String,Payload] =
    Decode.applicationXml(p => decode.fromXmlString(p).fold[Xor[Error,Try[Payload]]](
      err => Xor.Left(Error(err.getMessage)),
      value => Xor.Right(Try(value))
    ))

  val bodyXml = body.asXml[Payload]

  val xmlPayload = post(Root)(bodyXml)(payload => Ok(encode.toXml(payload)))

  serve(mkRoutes(Seq(xmlPayload)).service)

}

 */