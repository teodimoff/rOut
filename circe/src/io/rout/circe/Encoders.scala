package io.rout.circe

import com.twitter.io.Buf
import io.circe.{Encoder,Json}
import io.rout.Encode

trait Encoders {
  protected def print(json: Json): String

  implicit def encodeCirce[A](implicit e: Encoder[A]): Encode.ApplicationJson[A] =
    Encode.json(a=> Buf.Utf8(print(e(a))))
}
