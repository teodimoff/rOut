package io.rout.routing

import com.twitter.finagle.{Filter, Service}
import com.twitter.finagle.http._
import com.twitter.util.{Future, StorageUnit}
import com.twitter.conversions.storage._
import io.rout.{Output, ReqExt, ToResponse}

case class Routing(seq: Seq[RequestToService],
                   excHandler: Filter[Request,Response,Request,Response] = ExceptionBasic) {

  def :+(add: Seq[RequestToService]): Routing = copy(seq ++ add)

  def :+(add: RequestToService): Routing = copy(seq :+ add)

  def fileService(cacheSize: StorageUnit = 50.megabytes) = Assets(cacheSize).+:(seq)

  def fileService(cacheSize: String): Assets = fileService(StorageUnit.parse(cacheSize))

  def chainToAll(filter: Filter[Request,Response,Request,Response]) = seq.map(_.filter(filter))

  def add(assets: Assets) = :+(assets.seq)

  def withNotFound[CT <: String,A](html: A)(implicit tr: ToResponse.Aux[Output[A],CT]): Routing = handle {
    case NotFoundException => Output.payload(html,Status.NotFound)
  }

  def handle[CT <: String,A](fn: PartialFunction[Throwable,Output[A]] =
                             PartialFunction.empty[Throwable,Output[A]])(implicit tr: ToResponse.Aux[Output[A],CT]) =
    copy(seq,ExceptionFilter[CT,A](fn)(tr))

  def matchRequest(seq: Seq[RequestToService],request: Request): Future[Response] = {
    def x1x(rem: Seq[RequestToService]): Future[Response] = rem match {
      case Nil => NotFoundException.Future
      case x :: xs => x.request(request) match {
        case true => x.service(request)
        case false => x1x(xs)
      }
    }
    x1x(seq)
  }

  def service = excHandler andThen Service.mk[Request,Response](request => matchRequest(seq,request))

}

case object NotFoundException extends Exception {
  val Future = com.twitter.util.Future.exception(NotFoundException)
  val out = Output.empty(Status.NotFound)
}
