package io.rout.xml

import cats.data.Xor
import com.twitter.util.Try
import io.rout.{Decode, Error}
import scala.xml.XML
import io.rout.parse.xml.FromXml

trait Decoders {
  implicit def decodeXml[A](implicit fromXml: FromXml[A]): Decode.ApplicationXml[String,A] = Decode.applicationXml(i =>
    fromXml.fromXml(XML.loadString(i)).fold[Xor[Error,Try[A]]](
      err => Xor.Left(Error(err.getMessage)),
      value => Xor.Right(Try(value))
    ))
}
