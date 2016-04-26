package io.rout

import cats.Show
import cats.data.Xor
import com.twitter.io.Buf
import shapeless.Witness
import   io.rout.contentTypes._
import scalatags.Text.TypedTag

/**
 * An abstraction that is responsible for encoding the value of type `A`.
 */
trait Encode[A] {
  type ContentType <: String

  def apply(a: A): Buf
}

trait LowPriorityEncodeInstances {

  type Aux[A, CT <: String] = Encode[A] { type ContentType = CT }

  type ApplicationJson[A] = Aux[A, Application.Json]
  type TextHtml[A] = Aux[A, Text.Html]
  type TextPlain[A] = Aux[A, Text.Plain]

  def instance[A, CT <: String](fn: A => Buf): Aux[A, CT] =
    new Encode[A] {
      type ContentType = CT
      def apply(a: A): Buf = fn(a)
    }

  def json[A](fn: A => Buf): ApplicationJson[A] =
    instance[A, Application.Json](fn)

  //def text[A](fn: A => Buf): TextHtml[A] =
  //  instance[A, Text.Html](fn)

  def html[A](fn: A => Buf): TextHtml[A] =
    instance[A, Text.Html](fn)

  def scalaTag[A](fn: A => TypedTag[String]): TextHtml[A] =
    html[A](a=> Buf.Utf8(fn(a).render))

  implicit def encodeShow[A](implicit s: Show[A]): TextHtml[A] =
    html(a => Buf.Utf8(s.show(a)))
}

object Encode extends LowPriorityEncodeInstances {

  class Implicitly[A] {
    def apply[CT <: String](w: Witness.Aux[CT])(implicit
      e: Encode.Aux[A, CT]
    ): Encode.Aux[A, CT] = e
  }

  @inline def apply[A]: Implicitly[A] = new Implicitly[A]

  implicit def encodeUnit[CT <: String]: Aux[Unit, CT] =
    instance(_ => Buf.Empty)

  implicit def encodeBuf[CT <: String]: Aux[Buf, CT] =
    instance(identity)

  implicit val encodeExceptionAsTextPlain: TextHtml[Exception] =
    html(e => Buf.Utf8(Option(e.getMessage).getOrElse("")))

  implicit val encodeExceptionAsJson: ApplicationJson[Exception] =
    json(e => Buf.Utf8(s"""{"message": "${Option(e.getMessage).getOrElse("")}""""))

  implicit val encodeString: TextHtml[String] =
    html(Buf.Utf8.apply)

  implicit val enchtml: TextHtml[TypedTag[String]] = html(x=> Buf.Utf8(x.render))

  implicit def encodeXor[A, B, CT <: String](implicit
    ae: Encode.Aux[A, CT],
    be: Encode.Aux[B, CT]
  ): Encode.Aux[A Xor B, CT] = instance[A Xor B, CT](xor => xor.fold(ae.apply, be.apply))
}
