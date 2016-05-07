package io.rout

import algebra.Eq
import cats.laws._
import cats.laws.discipline._
import cats.std.AllInstances
import com.twitter.io.Buf
import org.scalacheck.{Prop, Arbitrary}
import org.typelevel.discipline.Laws
import shapeless.Witness

trait EncodeLaws[A, CT <: String] extends Laws with MissingInstances with AllInstances {

  def encode: Encode.Aux[A, CT]

  def roundTrip(a: A): IsEq[Buf] =
    encode(a) <-> Buf.Utf8(a.toString)

  def all(implicit A: Arbitrary[A], eq: Eq[A]): RuleSet = new DefaultRuleSet(
    name = "all",
    parent = None,
    "roundTrip" -> Prop.forAll { (a: A) => roundTrip(a) }
  )
}

object EncodeLaws {
  def texthtml[A: Encode.TextHtml]: EncodeLaws[A, Witness.`"text/html"`.T] =
    new EncodeLaws[A, Witness.`"text/html"`.T] {
      val encode: Encode.Aux[A, Witness.`"text/html"`.T] = implicitly[Encode.TextHtml[A]]
    }
}
