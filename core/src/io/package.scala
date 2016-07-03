package io

import com.twitter.finagle.Filter
import com.twitter.finagle.http.{Request, Response}
import io.rout.routing.{RequestToService, Rout}

package object rout extends Outputs with ReqReads with ValidationRules {
  val Status = com.twitter.finagle.http.Status

  implicit class FilterOpsRegular(val filter: Filter[Request,Response,Request,Response]) extends AnyVal {
    def andThen(rts: RequestToService): RequestToService = rts.filter(filter)
  }

  implicit class FilterOps[A](val filter: Filter[Request,Response,ReqExt[A],Response]) extends AnyVal {
    def andThen(f: Filter[Request,Response,ReqExt[A],Response] => RequestToService): RequestToService = f(filter)
  }
}
//rout package conflicts with path and routing
object routs extends Rout
