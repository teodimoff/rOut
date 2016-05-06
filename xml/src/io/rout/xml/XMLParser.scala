package io.rout.parse.xml

import java.util.UUID

import cats.{Foldable, Show}
import cats._
import implicits._
import cats.data.Xor
import com.twitter.util.{Eval, Return, Throw, Try}
import shapeless.{`::` => :#:, _}

import scala.annotation.implicitNotFound
import scala.xml._
import scala.language.higherKinds
import scala.reflect.ClassTag

trait Read[T] {
  def reads(s: String): Try[T]
}

trait ReadInstances {

  def tryDecode[A](f: String => A) = new Read[A] {
    def reads(s: String) = Try(f(s))
  }

  implicit val string = new Read[String] {
    def reads(s: String) = Return(s)
  }

  implicit val int: Read[Int]  = tryDecode(s=> s.toInt)

  implicit val long: Read[Long]  = tryDecode(s=> s.toInt)

  implicit val boolean: Read[Boolean]  = tryDecode(s=> s.toBoolean)

  implicit val double: Read[Double]  = tryDecode(s=> s.toDouble)

  implicit val float: Read[Float]  = tryDecode(s=> s.toFloat)

  implicit val decodeUUID: Read[UUID] = tryDecode(s =>
    if (s.length != 36) throw new IllegalArgumentException(s"Too long for UUID: ${s.length}")
    else UUID.fromString(s))

  implicit def decodeEnum[A](implicit ct: ClassTag[A]): Read[(A with Enumeration)#Value] = tryDecode(s =>
    (new Eval).inPlace[A with Enumeration](ct.runtimeClass.getCanonicalName.dropRight(1)).withName(s))
}

object Read extends ReadInstances {

  def apply[T](f: String => Try[T]): Read[T] = new Read[T] {
    override def reads(s: String) = f(s)
  }

  def reads[T](s: String)(implicit r: Read[T]): Try[T] = r reads s

}

@implicitNotFound(msg = "Implicit ToXml[${T}] not found. Try supplying an implicit instance of ToXml[${T}]")
trait ToXml[T] {
  def toXml(t: T): NodeSeq
  def toXml(t: T, rootLabel: String): NodeSeq = XML loadString s"<$rootLabel>${toXml(t)}</$rootLabel>"
}

@implicitNotFound(msg = "Implicit FroXml[${T}] not found. Try supplying an implicit instance of FromXml[${T}]")
trait FromXml[T] {
  def fromXmlString(xmlString: String) = fromXml(XML.load(xmlString))
  def fromXml(elem: NodeSeq): FromXml.Result[T]
}

object ToXml extends LabelledProductTypeClassCompanion[ToXml] {

  val Empty = NodeSeq.Empty
  def apply[T](f: T => NodeSeq): ToXml[T] = new ToXml[T] {
    override def toXml(t: T): NodeSeq = f(t)
  }

  def toXml[T](t: T)(implicit tx: ToXml[T]) = tx.toXml(t)

  /*Anything that can be shown can be xml*/
  implicit def ShowsToXml[T: Show]: ToXml[T] = ToXml[T]{ t: T =>
    val s = Show[T].show(t)
    scala.xml.Text(s.stripPrefix("\"").stripSuffix("\""))
  }

  implicit def OptionToXml[T](implicit tx: ToXml[T]) = new ToXml[Option[T]] {
    override def toXml(o: Option[T]) = o.fold(Empty)(tx.toXml)
    override def toXml(o: Option[T], rootLabel: String) = o.fold(Empty)(tx.toXml(_, rootLabel))
  }

  /*Anything that is foldable can be serialized to xml*/
  implicit def FoldableToXml[T, L[_]](implicit tx: ToXml[T], foldable: Foldable[L]): ToXml[L[T]] = new ToXml[L[T]] {
    override def toXml(list: L[T]): NodeSeq = list.foldLeft(Empty)(_ ++ tx.toXml(_))
    override def toXml(list: L[T], rootLabel: String): NodeSeq = list.foldLeft(Empty)(_ ++ tx.toXml(_, rootLabel))
  }

  object typeClass extends LabelledProductTypeClass[ToXml] {

    def emptyProduct: ToXml[HNil] = new ToXml[HNil] {
      def toXml(t: HNil) = Empty
    }

    def product[F, T <: HList](name: String, FHead: ToXml[F], FTail: ToXml[T]) = new ToXml[F :#: T] {
      override def toXml(hlist: F :#: T): NodeSeq = {
        hlist match {
          case head :#: tail =>
            val h = FHead toXml (head, name)
            val t = FTail toXml tail
            t ++ h

        }
      }
    }

    def project[F, G](instance: => ToXml[G], to: F => G, from: G => F) = new ToXml[F] {
      override def toXml(f: F): NodeSeq = instance.toXml(to(f))
    }
  }
}

object FromXml extends LabelledProductTypeClassCompanion[FromXml] {

  type Result[T] = Xor[Throwable,T]

  /*Anything with a Read typeclass instance can be deserialized from xml*/
  implicit def ReadsFromXml[T](implicit read: Read[T]): FromXml[T] = FromXml[T]{ n: NodeSeq =>

    read.reads(n.text) match {
      case Return(t) => Xor.Right(t)
      case Throw(e) => Xor.Left(new Throwable(s" [Error reading value: ${e.getMessage}]"))
    }
  }

  implicit def OptionFromXml[T](implicit fx: FromXml[T]): FromXml[Option[T]] = FromXml[Option[T]] { node: NodeSeq =>
    node match {
      case n if n.isEmpty =>  Xor.Right(None)
      case n              =>  Xor.Right(fx.fromXml(n).toOption)
    }
  }

  /*You know for like lists and sets and stuff*/
  implicit def MonoidFromXml[T, L[_]](implicit
    fx: FromXml[T],
    monoid: Monoid[L[T]],
    app: Applicative[L]
  ): FromXml[L[T]] = FromXml[L[T]]{ node: NodeSeq =>
    val zero = monoid.empty.right[Throwable]
    node.foldLeft(zero){ (listF, n) =>
      for {
        list <- listF
        f <- fx fromXml n
        v = app pure f
      } yield monoid.combine(list, v)
    }
  }

  def apply[T](f: NodeSeq => FromXml.Result[T]): FromXml[T] = new FromXml[T] {
    def fromXml(t: NodeSeq) = f(t)
  }

  object typeClass extends LabelledProductTypeClass[FromXml] {

    def emptyProduct: FromXml[HNil] = new FromXml[HNil] {
      def fromXml(t: NodeSeq) = Xor.Right(HNil)
    }

    def product[F, T <: HList](name: String, FHead: FromXml[F], FTail: FromXml[T]) = new FromXml[F :#: T] {
      override def fromXml(v: NodeSeq) = {
        val node = (v \ name)
        val result = for {
          h <- FHead fromXml node
          t <- FTail fromXml (v diff node)
        } yield h :: t

        result.leftMap(msg => new Throwable(s"/$name " + msg))
      }
    }

    def project[F, G](instance: => FromXml[G], to: F => G, from: G => F) = new FromXml[F] {
      override def fromXml(f: NodeSeq) = instance.fromXml(f).map(from)
    }
  }
}