package io

import com.twitter.finagle.Filter
import com.twitter.finagle.http.{Request, Response}
import io.rout.routing.{FilterRequestToService, RequestToService, Rout}

package object rout extends Outputs with ReqReads with ValidationRules {
  val Status = com.twitter.finagle.http.Status

  implicit class FilterOpsRegular(val filter: Filter[Request,Response,Request,Response]) extends AnyVal {
    def andThen(rts: RequestToService): RequestToService = rts.filter(filter)
  }

  implicit class FilterOps2[A](val filter: Filter[Request,Response,A,Response]) extends AnyVal {

    def andThen(frts: FilterRequestToService[ReqReadExt[A]]) = frts(filter.toExt)


    def toExt = Filter.mk[Request,Response,ReqReadExt[A],Response]((r,svc) => filter.andThen(a=> svc(ReqReadExt(a,r)))(r))

  }
}
//rout package conflicts with path and routing
object routs extends Rout
/*
package io

import com.twitter.finagle.Filter
import com.twitter.finagle.http.{Request, Response}
import io.rout.routing.{FilterRequestToService, RequestToService, Rout}

package object rout extends Outputs with ReqReads with ValidationRules {
  val Status = com.twitter.finagle.http.Status

  implicit class FilterOpsRegular(val filter: Filter[Request,Response,Request,Response]) extends AnyVal {
    def andThen(rts: RequestToService): RequestToService = rts.filter(filter)
  }

  implicit class FilterOps1[A](val filter: Filter[Request,Response,ReqExt[A],Response]) extends AnyVal {

    def andThen(frts: FilterRequestToService[ReqExt[A]]) = frts(filter)

  }

  implicit class FilterOps2[A](val filter: Filter[Request,Response,A,Response]) extends AnyVal {

    def toExt = Filter.mk[Request,Response,ReqReadExt[A],Response]((r,svc) => filter.andThen(a=> svc(ReqReadExt(a,r)))(r))

  }
}
//rout package conflicts with path and routing
object routs extends Rout

 */