package io.rout.circe

import cats.data.Xor
import com.twitter.io.Buf
import com.twitter.util.Try
import io.circe.Decoder
import io.circe.jawn.parseByteBuffer
import io.rout.{Decode, Error}

trait Decoders {

  implicit def decodeCirce[A](implicit d: Decoder[A]): Decode.ApplicationJson[Buf,A] = Decode.applicationJson(i =>
   parseByteBuffer(Buf.ByteBuffer.Shared.extract(i)).flatMap(json=> d.decodeJson(json)).fold[Xor[Error,Try[A]]](
     err => Xor.Left(Error(err.getMessage)),
     value => Xor.Right(Try(value))
   ))
}
