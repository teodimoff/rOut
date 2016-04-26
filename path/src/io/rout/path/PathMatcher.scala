package io.rout.path

import com.twitter.util.{Eval, Try}
import scala.reflect.ClassTag

trait PathMatcher[A] {
  def apply(s:String): Option[A]

}

case class Variant[ID,Ext,Va](id: ID,extension: Ext,variant: Va)

object PathMatcher {
  @inline def apply[A](implicit e: PathMatcher[A]) = e

  def instance[A](f: String => Option[A]): PathMatcher[A] = new PathMatcher[A] {
     def apply(s: String): Option[A] = f(s)
  }

  implicit val identity: PathMatcher[String] = instance(Some.apply)

  implicit val int: PathMatcher[Int] = instance(x => Some.apply(x.toInt))

  implicit val long: PathMatcher[Long] = instance(x => Some.apply(x.toLong))

  implicit val double: PathMatcher[Double] = instance(x => Some.apply(x.toDouble))

  implicit val float: PathMatcher[Float] = instance(x => Some.apply(x.toFloat))

  implicit val byte: PathMatcher[Byte] = instance(x => Some.apply(x.toByte))

  implicit val boolean: PathMatcher[Boolean] = instance(x => Some.apply(x.toBoolean))

  implicit def matchEnum[A](implicit ct: ClassTag[A]): PathMatcher[(A with Enumeration)#Value] = instance(s =>
    Try((new Eval).inPlace[A with Enumeration](ct.runtimeClass.getCanonicalName.dropRight(1)).withName(s)).toOption)

  implicit def variant[ID,Ext,Va](implicit
    id: PathMatcher[ID],
    ext: PathMatcher[Ext],
    va: PathMatcher[Va]) : PathMatcher[Variant[ID,Ext,Option[Va]]]= instance { x =>
    val arr = x.split(Array('.', ':'))
    (PathMatcher[ID].apply(arr.head), PathMatcher[Ext].apply(arr(1)), arr.lift(2)) match {
      case (Some(id_), Some(ext_), vaStringOption) =>
        Some(Variant(id_, ext_, vaStringOption.flatMap(vaString => PathMatcher[Va].apply(vaString))))
      case _ => None
    }
  }

}
/*

object Test  extends Enumeration {
  val large,medium,small,thumb = Value
/*
  case class Variant(a: Long,extension: String,variant: Option[String])

  implicit def variant : PathMatcher[Variant] = instance(x => Some.apply {
    val arr = x.split(Array('.',':'))
    Variant(arr.head.toLong,arr(1),arr.lift(2))
  })

 */

 //val p = Path(Root / "media" / Match[Variant])

}
 */