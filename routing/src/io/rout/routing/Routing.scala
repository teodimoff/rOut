package io.rout.routing


import com.twitter.finagle.Service
import com.twitter.finagle.http._
import com.twitter.util.{Future, StorageUnit}
import com.twitter.conversions.storage._

case class Routing(seq: Seq[RequestToService],FNF: Future[Response]) {

  def :+(add: Seq[RequestToService]): Routing = copy(seq ++ add, FNF)

  def :+(add: RequestToService): Routing = copy(seq :+ add, FNF)

  def fileService(cacheSize: StorageUnit = 50.megabytes) = Assets(cacheSize).+:(seq)

  def fileService(cacheSize: String): Assets = fileService(StorageUnit.parse(cacheSize))

  def add(assets: Assets) = :+(assets.seq)

  def withNotFound(html: String): Routing = {
    val response = Response(Status.NotFound)
    response.contentType = "text/html; charset=utf8"
    response.contentString = html
     Routing(seq,Future(response))
  }

  def matchRequest(seq: Seq[RequestToService],request: Request): Future[Response] = {
    def x1x(rem: Seq[RequestToService]): Future[Response] = rem match {
      case Nil =>  FNF
      case x :: xs => x.request(request) match {
        case true => x.service(request)
        case false => x1x(xs)
      }
    }
    x1x(seq)
  }

  def service =  ExceptionFilter andThen Service.mk[Request,Response](request => matchRequest(seq,request))

}
