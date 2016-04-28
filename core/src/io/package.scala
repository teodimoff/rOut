package io

import com.twitter.finagle.Filter
import com.twitter.finagle.http.{Request, Response}
import io.rout.routing.{RequestToService, Rout}

/**
 *
 */
package object rout extends Outputs with ReqReads with ValidationRules{
  implicit class FilterOpsRegular(val filter: Filter[Request,Response,Request,Response]) extends AnyVal {
    def andThen(rts: RequestToService): RequestToService = rts.filter(filter)
  }

  implicit class FilterOps[A](val filter: Filter[Request,Response,ReqExt[A],Response]) extends AnyVal {
    def andThen(f: Filter[Request,Response,ReqExt[A],Response] => RequestToService): RequestToService = f(filter)
  }
}

object routs extends Rout