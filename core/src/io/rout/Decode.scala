package io.rout

import java.util.UUID
import cats.data.Xor
import com.twitter.util.{Eval, Try}
import shapeless.{::, Generic, HNil, Witness}
import scala.language.implicitConversions
import scala.language.higherKinds
import scala.reflect.ClassTag
import io.rout.contentTypes._
/**
 * An abstraction that is responsible for decoding the request of type `A`.
 */
trait Decode[I,A] {
  type ContentType <: String
  def apply(i: I): Decode.Result[A]

}

trait LowPriorityDecodeInstances {

  type Aux[I,A,CT <: String] = Decode[I,A]{ type ContentType = CT }

  type ApplicationJson[I,A] = Aux[I,A,Application.Json]

  type ApplicationXml[I,A] = Aux[I,A,Application.Xml]

  type TextPlain[I,A] = Aux[I,A,Text.Plain]

  /**
   * Creates an instance for a given type.
   */
  def instance[I,A,CT <: String](f: I => Decode.Result[A]): Aux[I,A,CT] = new Decode[I,A]{
    type ContentType = CT
    def apply(i: I): Decode.Result[A] = f(i)
  }

  def applicationJson[I,A](fn: I => Decode.Result[A]): ApplicationJson[I,A] =
    instance[I,A,Application.Json](fn)

  def applicationXml[I,A](fn: I => Decode.Result[A]): ApplicationXml[I,A] =
    instance[I,A,Application.Xml](fn)

  def textPlain[I,A](fn: I => Decode.Result[A]): TextPlain[I,A] =
    instance[I,A,Text.Plain](fn)

  /**
   * Creates a [[Decode]] from [[shapeless.Generic]].
   *
   * Note: This is mostly a workaround for `ReqRead[String].as[CaseClassOfASingleString]`,
   *       because by some reason, compiler doesn't pick `ValueReaderOps` for `ReqRead[String]`.
   */
  implicit def decodeFromGeneric[A,CT <: String](implicit
   gen: Generic.Aux[A, String :: HNil],
   w: Witness.Aux[CT]
  ): Decode.Aux[String,A,CT] = instance(s => Xor.Right(Try(gen.from(s :: HNil))))
}

object Decode extends LowPriorityDecodeInstances {

  type Result[A] = Xor[Error, Try[A]]

  class Implicitly[I,A]{
    def apply[CT <: String](w: Witness.Aux[CT])(implicit
      d: Decode.Aux[I,A,CT]): Decode.Aux[I,A,CT] = d
  }

  implicit def decodeEnum[I,A](implicit ct: ClassTag[A]): Decode[String,(A with Enumeration)#Value] = tryDecode(s =>
    (new Eval).inPlace[A with Enumeration](ct.runtimeClass.getCanonicalName.dropRight(1)).withName(s))

  private def tryDecode[I,A](f: I => A): Decode.TextPlain[I,A] =
    textPlain(s =>  Xor.Right(Try(f(s))))

  /**
   * Returns an instance for a given type.
   */
 @inline def apply[I,A]: Implicitly[I,A] = new Implicitly[I,A]


  /**
   * A [[Decode]] instance for `String`.
   */
  implicit val decodeString: TextPlain[String,String] =
    tryDecode(s => s)

  /**
   * A [[Decode]] instance for `Int`.
   */
  implicit val decodeInt: TextPlain[String,Int] = tryDecode(s => s.toInt)

  /**
   * A [[Decode]] instance for `Long`.
   */
  implicit val decodeLong: TextPlain[String,Long] = tryDecode(s => s.toLong)

  /**
   * A [[Decode]] instance for `Float`.
   */
  implicit val decodeFloat: TextPlain[String,Float] = tryDecode(s => s.toFloat)

  /**
   * A [[Decode]] instance for `Double`.
   */
  implicit val decodeDouble: TextPlain[String,Double] = tryDecode(s => s.toDouble)

  /**
   * A [[Decode]] instance for `Boolean`.
   */
  implicit val decodeBoolean: TextPlain[String,Boolean] = tryDecode(s => s.toBoolean)

  /**
   * A [[Decode]] instance for `UUID`.
   */
  implicit val decodeUUID: TextPlain[String,UUID] = tryDecode(s =>
    if (s.length != 36) throw new IllegalArgumentException(s"Too long for UUID: ${s.length}")
    else UUID.fromString(s))
}

