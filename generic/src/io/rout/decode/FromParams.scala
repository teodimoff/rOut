package io.rout.generic.decoding


import shapeless.labelled._
import shapeless._

import scala.reflect.ClassTag
import scala.language.implicitConversions
import scala.language.higherKinds
import com.twitter.finagle.http.Cookie
import io.rout._

/**
 * A type classes empowering a generic derivation of [[io.rout.ReqRead]]s from query string params.
 */

trait FromParams[L <: HList] {
  def reader: ReqRead[L]
}

object FromParamsExtract extends Poly1 {

   case class FromMap[K,V](key: Seq[K], value: Seq[V]){
    def toMap = (key zip value).toMap
  }

  implicit val cookieRR: Case.Aux[String,ReqRead[Cookie]] = at[String](key => cookie(key))

  implicit val cookieRROption: Case.Aux[String,ReqRead[Option[Cookie]]] = at[String](key => cookieOption(key))

  implicit def nested[Repr <: HList,V](implicit
    gen: LabelledGeneric.Aux[V, Repr],
    fp:  Lazy[FromParams[Repr]]
  ): Case.Aux[String,ReqRead[V]] = at[String]{ key =>
    derive[V].fromParams
  }

  implicit def nestedOption[Repr <: HList,V](implicit
    gen: LabelledGeneric.Aux[V, Repr],
    fp:  Lazy[FromParams[Repr]]
  ): Case.Aux[String,ReqRead[Option[V]]] = at[String]{ key =>
    derive[V].fromParams.lift
  }

  implicit def optionalExtract[V](implicit
   dh: Decode.TextPlain[String,V],
   ct: ClassTag[V]
  ): Case.Aux[String,ReqRead[Option[V]]] = at[String]{ key =>
   paramOption(key).asText(dh,ct)
  }

  implicit def map[Repr <: HList,K,V](implicit
    gen: LabelledGeneric.Aux[FromMap[K,V], Repr],
    fp:  Lazy[FromParams[Repr]]
  ): Case.Aux[String,ReqRead[Map[K,V]]] = at[String]{ key =>
    derive[FromMap[K,V]].fromParams.map(_.toMap)
  }

  implicit def mapOption[Repr <: HList,K,V](implicit
    gen: LabelledGeneric.Aux[FromMap[K,V], Repr],
    fp:  Lazy[FromParams[Repr]]
  ): Case.Aux[String,ReqRead[Option[Map[K,V]]]] = at[String]{ key =>
    derive[FromMap[K,V]].fromParams.map(_.toMap).lift
  }

  implicit def seqExtractor[V](implicit
   dh: Decode.TextPlain[String,V],
   ct: ClassTag[V]
  ): Case.Aux[String, ReqRead[Seq[V]]] = at[String] { key =>
    paramsNonEmpty(key).asText(dh, ct)
  }

  implicit def seqExtractorOption[V](implicit
   dh:Decode.TextPlain[String,V],
   ct: ClassTag[V]
  ): Case.Aux[String, ReqRead[Option[Seq[V]]]] = at[String] { key =>
    params(key).asText(dh, ct).map{
    case Nil => None
    case seq => Some(seq)
    }
  }

  implicit def extract[V](implicit
   dh: Decode.TextPlain[String,V],
   ct: ClassTag[V]
  ): Case.Aux[String,ReqRead[V]] = at[String]{key =>
   param(key).asText(dh,ct)
  }
}

object FromParams extends Cases {

  implicit val hnilFromParams: FromParams[HNil] = new FromParams[HNil] {
    def reader: ReqRead[HNil] = ReqRead.value(HNil)
  }

  implicit def hconsFromParams[HK <: Symbol, HV, T <: HList](implicit
    key: Witness.Aux[HK],
    fpt: FromParams[T],
    hc: Case1.Aux[FromParamsExtract.type ,String,ReqRead[HV]]
  ): FromParams[FieldType[HK, HV] :: T] = new FromParams[FieldType[HK, HV] :: T] {
    def reader: ReqRead[FieldType[HK, HV] :: T] =
      hc(key.value.name).map(field[HK](_)) :: fpt.reader
  }

}
