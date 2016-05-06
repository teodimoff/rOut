package io.rout.xml

import com.twitter.io.Buf
import io.rout.Encode
import io.rout.parse.xml.ToXml

trait Encoders {
  implicit def encodeXml[T](implicit toXml: ToXml[T]): Encode.ApplicationXml[T] = Encode.xml(t =>
    Buf.Utf8(toXml.toXml(t).toString()))
}
