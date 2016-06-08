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

/*
object ExcFilter{
  def apply[CT<: String,A](fn: PartialFunction[Throwable,Output[A]] =
                         PartialFunction.empty[Throwable,Output[A]])(implicit tr: ToResponse.Aux[Output[A],CT]) =
    ExceptionFilter(fn)(tr)
}

 */
case class ExceptionFilter[CT,A](fn: PartialFunction[Throwable,Output[A]])
                              (implicit tr: ToResponse.Aux[Output[A],CT]) extends SimpleFilter[Request,Response] {

  def apply(request: Request, service: Service[Request, Response]) = {
    service(request) handle fn.orElse[Throwable,Output[A]] {
      case error =>
        error match {
          case NotFoundException => Output.empty(Status.NotFound)
          case _ => Output.empty(Status.Locked)
        }
    }.andThen { x =>
      val r = tr(x)
      r.charset = Charsets.Utf8.toString
      r.status = x.status
      r
    }
  }

}