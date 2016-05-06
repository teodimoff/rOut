package io.rout.generic


import io.rout.parse.xml._
import shapeless._

object xml {

/*
  object auto {

  implicit def deriveEncoder[T,LKV](implicit
    gen:LabelledGeneric.Aux[T,LKV],
    toXml: Lazy[ToXml.Wrap[LKV]]
  ): ToXml[T] = ToXml.deriveInstance(gen,toXml)

  implicit def deriveDecoder[T,LKV](implicit
    gen:LabelledGeneric.Aux[T,LKV],
    fromXml: Lazy[FromXml.Wrap[LKV]]
  ): FromXml[T] = FromXml.deriveInstance(gen,fromXml)
}
 */

object semiauto {

  class Derive[T] {
   def xmlEncoder[LKV](implicit
    gen:LabelledGeneric.Aux[T,LKV],
    toXml: Lazy[ToXml.Wrap[LKV]]
  ): ToXml[T] = ToXml.deriveInstance(gen,toXml)

   def xmlDecoder[LKV](implicit
    gen:LabelledGeneric.Aux[T,LKV],
    fromXml: Lazy[FromXml.Wrap[LKV]]
  ): FromXml[T] = FromXml.deriveInstance(gen,fromXml)
  }

  def derive[T] = new Derive[T]
}

}
