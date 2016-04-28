package io.rout.routing


import com.twitter.finagle.Service
import com.twitter.finagle.http._
import com.twitter.util.Future

class Routing(seq: Seq[RequestToService],futureNotFound: Future[Response]) {

  def :+(add: Seq[RequestToService]): Routing =
    new Routing(seq ++ add, futureNotFound)

  def :+(add: RequestToService): Routing =
    new Routing(seq :+ add, futureNotFound)

  def asset1(prefix: String) =
    new Routing(seq :+ new Assets(prefix).asset1,futureNotFound)

  def asset2(prefix: String) =
    new Routing(seq :+ new Assets(prefix).asset2,futureNotFound)

  def asset3(prefix: String) =
    new Routing(seq :+ new Assets(prefix).asset3,futureNotFound)

  def asset4(prefix: String) =
    new Routing(seq :+ new Assets(prefix).asset4,futureNotFound)

  def asset5(prefix: String) =
    new Routing(seq :+ new Assets(prefix).asset5,futureNotFound)

  def asset(prefix: String,depth: Int) =
    new Routing(seq ++ new Assets(prefix).assetDepth(depth),futureNotFound)

  def debugAssets =  new Routing(seq ++ Assets.debug,futureNotFound)

  def withNotFound(html: String): Routing = {
    val response = Response(Status.NotFound)
    response.contentType = "text/html; charset=utf8"
    response.contentString = html
    new Routing(seq,Future(response))
  }

  def matchRequest(seq: Seq[RequestToService],request: Request): Future[Response] = {
    def x1x(rem: Seq[RequestToService]): Future[Response] = rem match {
      case Nil =>  futureNotFound
      case x :: xs => x.request(request) match {
        case true => x.service(request)
        case false => x1x(xs)
      }
    }
    x1x(seq)
  }

  def service =  ExceptionFilter andThen Service.mk[Request,Response](request => matchRequest(seq,request))

  //def service =  Service.mk[Request,Response](request => matchRequest(seq,request))

}
