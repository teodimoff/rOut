package io.rout.generic


import shapeless.ops.function.FnFromProduct
import shapeless.ops.record.RemoveAll
import shapeless.{HList, LabelledGeneric, Lazy}
import io.rout._
/**
 * Request reader methods for getting incomplete instances
 */
package object decoding {

class GenericDerivation[A] {
  /**
   * Derive all parameters from  instance of [[io.rout.ReqRead]] for a given type `A`.
   */
  def fromParams[Repr <: HList](implicit
   gen: LabelledGeneric.Aux[A, Repr],
   fp:  Lazy[FromParams[Repr]]
  ): ReqRead[A] = fp.value.reader.map(gen.from)

  /**
   * Incomlete instance of [[io.rout.ReqRead]] for a given type `A`.
   */
  def incomplete[P <: HList, C, T <: HList, R <: HList](implicit
   ffp:  FnFromProduct.Aux[P => C, A],
   gen:  LabelledGeneric.Aux[C, T],
   removeAll:  RemoveAll.Aux[T, P, (P, R)],
   fp:  Lazy[FromParams[R]]
  ): ReqRead[A] =
    fp.value.reader.map(r => ffp(p => gen.from(removeAll.reinsert((p, r)))))


  /**
   * Patch instance of [[io.rout.ReqRead]] for a given type `A`.
   */
  def patch[R <: HList, O <: HList](implicit
    gen: LabelledGeneric.Aux[A, R],
    patch: PatchWithOptions.Aux[R, O],
    fp: Lazy[FromParams[O]]
  ): ReqRead[A => A] =
    fp.value.reader.map(o => a => gen.from(patch(gen.to(a), o)))

}

  /**
   * Generically derive a very basic instance of [[io.rout.ReqRead]] for a given type `A`.
   */

  def derive[A]: GenericDerivation[A] = new GenericDerivation[A]

}
