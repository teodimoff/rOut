package io.rout.routing

import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.finagle.http._
import io.rout.ToResponse

case class ExcpFn(status: Status,message: String)

object ExcFilter {
  def apply[CT<: String](fn: PartialFunction[Throwable,ExcpFn] =
                         PartialFunction.empty[Throwable,ExcpFn])(implicit tr: ToResponse.Aux[ExcpFn,CT]) =
    ExceptionFilter(fn)(tr.apply)
}

case class ExceptionFilter(fn: PartialFunction[Throwable,ExcpFn])
                                       (tr: ExcpFn => Response) extends SimpleFilter[Request,Response] {
  def apply(request: Request, service: Service[Request, Response]) = {
    service(request) handle fn.orElse[Throwable,ExcpFn] {
      case error =>
        error match {
          case _ => ExcpFn(Status.Forbidden,"")
        }
    }.andThen { x =>
      val r = tr(x)
      r.status = x.status
      r
    }
  }
}

