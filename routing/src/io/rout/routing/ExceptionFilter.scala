package io.rout.routing

import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.finagle.http._
import com.twitter.io.Charsets
import io.rout.{Output, ToResponse}

object ExceptionBasic extends SimpleFilter[Request,Response] {
  def apply(request: Request, service: Service[Request, Response]) =
    service(request) handle {
      case error =>
        val response = request.response
        response.status =  Status.Forbidden
       response
    }
}

case class ExceptionFilter[CT,A](fn: PartialFunction[Throwable,Output[A]])
                              (implicit tr: ToResponse.Aux[Output[A],CT]) extends SimpleFilter[Request,Response] {

  def apply(request: Request, service: Service[Request, Response]) = {
    service(request) handle fn.orElse[Throwable,Output[A]] {
      case error =>
        error match {
          case NotFoundException => NotFoundException.out
          case _ => Output.empty(Status.Forbidden)
        }
    }.andThen { x =>
      val r = tr(x)
      r.charset = Charsets.Utf8.toString
      r.status = x.status
      r
    }
  }

}