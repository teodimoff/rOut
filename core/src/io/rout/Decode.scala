package io.rout

import java.util.UUID
import cats.data.Xor
import com.twitter.util.{Return, Eval, Try}
import shapeless.{Lazy, ::, Generic, HNil}
import scala.collection.generic.{IsTraversableLike, CanBuildFrom}
import scala.language.implicitConversions
import scala.language.higherKinds
import scala.reflect.ClassTag

/**
 * An abstraction that is responsible for decoding the request of type `A`.
 */
trait Decode[A] {

  def apply(s: String): Decode.Result[A]

}

trait LowPriorityDecodeInstances {

  /**
   * Creates an instance for a given type.
   */
  def instance[A](f: String => Decode.Result[A]): Decode[A] = new Decode[A] {
    def apply(s: String): Decode.Result[A] = f(s)

  }

  /**
   * Creates a [[Decode]] from [[shapeless.Generic]].
   *
   * Note: This is mostly a workaround for `ReqRead[String].as[CaseClassOfASingleString]`,
   *       because by some reason, compiler doesn't pick `ValueReaderOps` for `ReqRead[String]`.
   */
  implicit def DecodeFromGeneric[A](implicit
   gen: Generic.Aux[A, String :: HNil]
  ): Decode[A] = instance(s => Xor.Right(Try(gen.from(s :: HNil))))
}

object Decode extends LowPriorityDecodeInstances {

  type Result[A] = Xor[Error, Try[A]]

  implicit def decodeEnum[A](implicit ct: ClassTag[A]): Decode[(A with Enumeration)#Value] = tryDecode(s =>
    (new Eval).inPlace[A with Enumeration](ct.runtimeClass.getCanonicalName.dropRight(1)).withName(s))

  private def tryDecode[A](f: String => A): Decode[A] =
    instance(s =>  Xor.Right(Try(f(s))))

  /**
   * Returns an instance for a given type.
   */
 @inline def apply[A](implicit dr: Decode[A]): Decode[A] = dr


  /**
   * A [[Decode]] instance for `String`.
   */
  implicit val decodeString: Decode[String] =
    tryDecode(s => s)

  /**
   * A [[Decode]] instance for `Int`.
   */
  implicit val decodeInt: Decode[Int] = tryDecode(s => s.toInt)

  /**
   * A [[Decode]] instance for `Long`.
   */
  implicit val decodeLong: Decode[Long] = tryDecode(s => s.toLong)

  /**
   * A [[Decode]] instance for `Float`.
   */
  implicit val decodeFloat: Decode[Float] = tryDecode(s => s.toFloat)

  /**
   * A [[Decode]] instance for `Double`.
   */
  implicit val decodeDouble: Decode[Double] = tryDecode(s => s.toDouble)

  /**
   * A [[Decode]] instance for `Boolean`.
   */
  implicit val decodeBoolean: Decode[Boolean] = tryDecode(s => s.toBoolean)

  /**
   * A [[Decode]] instance for `UUID`.
   */
  implicit val decodeUUID: Decode[UUID] = instance(s =>
    if (s.length != 36) Xor.Left(throw new IllegalArgumentException(s"Too long for UUID: ${s.length}"))
    else Xor.Right(Try(UUID.fromString(s)))
  )
}

/*
    implicit def decodeCanBuildFrom[A, C[_]](implicit
   dr: Decode[A],
   cbf: CanBuildFrom[Nothing, A, C[A]]): Decode[C[A]] = instance { s =>
    val seq = s.split(",").toSeq
      val builder = cbf()
      seq.flatMap(x => dr.apply(x).toOption).flatMap(_.toOption) map (x => builder += x)
      Xor.Right(Try(builder.result()))
  }

  implicit def decodeCanBuildFrom[A, C[_]](implicit
   dr: Decode[A],
   cbf: CanBuildFrom[Nothing, A, C[A]]): Decode[C[A]] = instance { s =>
      val builder = cbf()
      dr.apply(s).toOption.flatMap(_.toOption) map (x => builder += x)
      Xor.Right(Try(builder.result()))
  }

  implicit def decodeOption[A](implicit dr: Decode[A]): Decode[Option[A]] = instance(s =>
  dr(s) match {
    case Xor.Right(Return(Nil)) => rightNone
    case Xor.Right(a) => Xor.right(Try(a.toOption))
    case Xor.Left(df) => rightNone
  })

  implicit def decodeFromOption[A](implicit
     dr: Decode[Option[A]]
  ): Decode[A] = implicitly[Decode[A]](instance(s =>
    dr(s) match {
    case Xor.Right(Return(Some(a))) => Xor.right(Try(a))
    case Xor.Left(df) =>  Xor.left(df)
  }))

    private[this] val rightNone: Xor[Error, Try[Option[Nothing]]] = Xor.right(Try(None))

 */