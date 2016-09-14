package io.rout.routing

import com.twitter.finagle.{Filter, Service}
import com.twitter.finagle.http.{Request, Response}
import io.rout.ReqRead

case class FilterRequestToService[X](filter: Filter[Request,Response,X,Response] => RequestToService){

  def apply(x: Filter[Request,Response,X,Response]):  RequestToService = filter(x)

  def apply(x: ReqRead[X]): RequestToService = filter(x.toFilter)

}

case class RequestToService(request: Request => Boolean,service: Service[Request,Response]) extends  Routable { self =>

  val routes = Seq(self)

  def filter(filter: Filter[Request,Response,Request,Response]) =
    copy(service = filter andThen service)

  def withFilter(filter: Filter[Request,Response,Request,Response]) =
    copy(service = filter andThen service)
}
