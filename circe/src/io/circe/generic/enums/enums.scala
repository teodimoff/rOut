package io.circe.generic

import io.circe._
import shapeless._

object enums {

  def time[A](a: => A) = {
    val now = System.nanoTime()
    val result = a
    val nano = System.nanoTime() - now
    val micros = nano / 1000
    val seconds = micros.toFloat / 1000 / 1000
    println("%d nanoseconds".format(nano),"%d microseconds".format(micros),"%.3f seconds".format(seconds))
    result
  }

  implicit def decodeEnum[A <: Enumeration](implicit w: Witness.Aux[A]) = new Decoder[A#Value]{
      def apply(c: HCursor): io.circe.Decoder.Result[A#Value] =
       Decoder.decodeString.map(s => implicitly[A](w.value).withName(s)).apply(c)
    }

  implicit def ecodeEnum[A <: Enumeration] = new Encoder[A#Value] {
    def apply(a: A#Value) = Encoder.encodeString.apply(a.toString)
  }
}
/*
:paste
import io.circe.generic.auto._
import io.circe.generic.enums._
import com.borsell.common.adt._

val db = store2.devdb.dev

case class Test(id: Long,time: Long,title: String,desc:String,media: Option[Seq[Long]],a: String,b: String)

  def time[A](a: => A) = {
    val now = System.nanoTime()
    val result = a
    val nano = System.nanoTime() - now
    val micros = nano / 1000
    val seconds = micros.toFloat / 1000 / 1000
    println("%d nanoseconds".format(nano),"%d microseconds".format(micros),"%.3f seconds".format(seconds))
    result
  }

 */
