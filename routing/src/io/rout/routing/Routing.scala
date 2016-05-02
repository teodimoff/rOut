package io.rout.routing


import com.twitter.finagle.Service
import com.twitter.finagle.http._
import com.twitter.util.{Future, StorageUnit}
import io.rout.file.FileOps
import com.twitter.conversions.storage._

case class Routing(seq: Seq[RequestToService],FNF: Future[Response]) {

  lazy val fileCache = withCacheSize()

  def :+(add: Seq[RequestToService]): Routing =
     Routing(seq ++ add, FNF)

  def :+(add: RequestToService): Routing =
     Routing(seq :+ add, FNF)

  def asset1(prefix: String) =
     Routing(seq :+ Assets(prefix,fileCache).asset1,FNF)

  def asset2(prefix: String) =
     Routing(seq :+ Assets(prefix,fileCache).asset2,FNF)

  def asset3(prefix: String) =
     Routing(seq :+ Assets(prefix,fileCache).asset3,FNF)

  def asset4(prefix: String) =
     Routing(seq :+ Assets(prefix,fileCache).asset4,FNF)

  def asset5(prefix: String) =
     Routing(seq :+ Assets(prefix,fileCache).asset5,FNF)

  def asset(prefix: String,depth: Int) =
     Routing(seq ++ Assets(prefix,fileCache).assetDepth(depth),FNF)

  def debugAssets = Routing(seq ++ Assets("",fileCache).addDebug(),FNF)

  def withCacheSize(size: StorageUnit = 50.megabyte) = FileOps(size)

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
