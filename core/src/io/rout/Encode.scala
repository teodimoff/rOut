package io.rout

import cats.Show
import cats.data.Xor
import com.twitter.io.Buf
import shapeless.Witness
import io.rout.contentTypes._

import scala.xml.NodeSeq
import scalatags.Text.TypedTag
import scalatags.Pretty._
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
  type ApplicationXml[A] = Aux[A, Application.Xml]
  type TextHtml[A] = Aux[A, Text.Html]
  type TextPlain[A] = Aux[A, Text.Plain]

  def instance[A, CT <: String](fn: A => Buf): Aux[A, CT] =
    new Encode[A] {
      type ContentType = CT
      def apply(a: A): Buf = fn(a)
    }

  def xml[A](fn: A => Buf): ApplicationXml[A] =
    instance[A, Application.Xml](fn)

  def json[A](fn: A => Buf): ApplicationJson[A] =
    instance[A, Application.Json](fn)

  def textPlain[A](fn: A => Buf): TextPlain[A] =
    instance[A, Text.Plain](fn)

  def html[A](fn: A => Buf): TextHtml[A] =
    instance[A, Text.Html](fn)

  def scalaTag[A](fn: A => TypedTag[String])(implicit prettyPrint:Boolean): TextHtml[A] =
    html[A](x=> Buf.Utf8(if(prettyPrint) fn(x).pretty() else  fn(x).render))

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

  implicit val encodeExceptionXml: ApplicationXml[Exception] =
    xml(e => Buf.Utf8(Option(e.getMessage).getOrElse("")))

  implicit val encodeString: TextHtml[String] =
    html(Buf.Utf8.apply)

  //implicit val encodehtml: TextHtml[TypedTag[String]] = html(x=> Buf.Utf8(x.render))

  implicit def encodehtml(implicit prettyPrint:Boolean): Encode.TextHtml[TypedTag[String]] =
    Encode.html(x=> Buf.Utf8(if(prettyPrint)x.pretty() else x.render))

  implicit val encodeXml: ApplicationXml[NodeSeq] = xml(x=> Buf.Utf8(x.toString()))

  implicit def encodeXor[A, B, CT <: String](implicit
    ae: Encode.Aux[A, CT],
    be: Encode.Aux[B, CT]
  ): Encode.Aux[A Xor B, CT] = instance[A Xor B, CT](xor => xor.fold(ae.apply, be.apply))
}
