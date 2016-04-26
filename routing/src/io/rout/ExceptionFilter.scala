package io.rout.routing

import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.finagle.http._

/**
 * Created by betepahos on 31.01.16.
 */

object ExceptionFilter extends SimpleFilter[Request,Response] {
  def apply(request: Request, service: Service[Request, Response]) = {
    service(request) handle {
      case error =>
        val statusCode = error match {
                    case _: IllegalArgumentException => Status.NotAcceptable
                    case _: Throwable => Status.NotFound
                    case _ => Status.Forbidden
        }
        val errorResponse = Response(Version.Http11, statusCode)
        error match {
          case e: Throwable => errorResponse.write(e.getMessage)
          case _ => errorResponse.write(error.toString)
        }
        errorResponse
    }
  }
}

