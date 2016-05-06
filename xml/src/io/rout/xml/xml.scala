package io.rout.generic

import cats.data.Xor
import com.twitter.io.Buf
import com.twitter.util.Try
import io.rout.{Decode, Encode, Error}
import io.rout.parse.xml._
import shapeless._
import scala.xml.XML

object xml {

object semiauto {

  class Encoder[T]{
    def encodeXml(implicit toXml: ToXml[T]): Encode.ApplicationXml[T] = Encode.xml(t =>
      Buf.Utf8(toXml.toXml(t).toString()))

    def derive[LKV](implicit
      gen:LabelledGeneric.Aux[T,LKV],
      toXml: Lazy[ToXml.Wrap[LKV]]
    ): ToXml[T] = ToXml.deriveInstance(gen,toXml)
  }

  class Decoder[T]{
     def decodeXml(implicit fromXml: FromXml[T]): Decode.ApplicationXml[String,T] = Decode.applicationXml(i =>
      fromXml.fromXml(XML.loadString(i)).fold[Xor[Error,Try[T]]](
        err => Xor.Left(Error(err.getMessage)),
        value => Xor.Right(Try(value))
      ))

   def derive[LKV](implicit
    gen:LabelledGeneric.Aux[T,LKV],
    fromXml: Lazy[FromXml.Wrap[LKV]]
  ): FromXml[T] = FromXml.deriveInstance(gen,fromXml)

  }

  def decode[T] = new Decoder[T]

  def encode[T] = new Encoder[T]
}

}
