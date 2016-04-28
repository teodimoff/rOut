package io.rout.routing

import com.twitter.finagle.{Service, ServiceException, SimpleFilter}
import com.twitter.finagle.http._

object ExceptionFilter extends SimpleFilter[Request,Response] {
  def apply(request: Request, service: Service[Request, Response]) = {
    service(request) handle {
      case error =>
        val statusCode = error match {
                    case _: ServiceException => Status.NotFound
                    case _: IllegalArgumentException => Status.NotAcceptable
                    case _: Throwable => Status.NotFound
                    case _ => Status.Forbidden
        }
        val errorResponse = Response(Version.Http11, statusCode)
        error match {
          case s: ServiceException => errorResponse.write(s.fillInStackTrace().toString)
          case t: Throwable => errorResponse.write(t.getMessage)
          case _ => errorResponse.write(error.toString)
        }
        errorResponse
    }
  }
}

