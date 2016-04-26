package io.rout.routing

import com.twitter.finagle.http.Response
import io.rout.path.Path
import com.twitter.util.Future
import io.rout.ReqReads
import io.rout.file.FileOps

/**
  * Created by betepahos on 20.04.16.
  */
case class Assets(assetPrefix: String) extends Rout {
  import Assets._
  private val drop = Path(assetPrefix).toList.size

  val asset1 = get(Root / assetPrefix / path[String]).service(r =>
    FileOps(Path(r.path).drop(drop).toString).response(Future(notFound)))

  val asset2 = get(Root / assetPrefix / path[String] / path[String]).service(r =>
    FileOps(Path(r.path).drop(drop).toString).response(Future(notFound)))

  val asset3 = get(Root / assetPrefix / path[String] / path[String] / path[String]).service(r =>
    FileOps(Path(r.path).drop(drop).toString).response(Future(notFound)))

  val asset4 = get(Root / assetPrefix / path[String] / path[String] / path[String] / path[String]).service(r =>
    FileOps(Path(r.path).drop(drop).toString).response(Future(notFound)))

  val asset5 = get(Root / assetPrefix / path[String] / path[String] / path[String] / path[String] / path[String]).service(r =>
    FileOps(Path(r.path).drop(drop).toString).response(Future(notFound)))

  def assetDepth(depth: Int) = depth match {
    case 1 => Seq(asset1,asset2)
    case 2 => Seq(asset1,asset2)
    case 3 => Seq(asset1,asset2,asset3)
    case 4 => Seq(asset1,asset2,asset3,asset4)
    case 5 => Seq(asset1,asset2,asset3,asset4,asset5)
  }
}
object Assets extends Rout with ReqReads {
  //use path param to read file from local filesystem eq :8080/local/pic.jpg?path=/home/teodimoff/
  private val localAsset = get(Root / "local" / path[String]).service(param("path"))((file,path) =>
    FileOps.static(path + "/" + file,Future(notFound)))

  private val seeAllCached = get(Root / "see").service(request => FileOps.seeAll())

  def debug = Seq(localAsset,seeAllCached)

  implicit class FileOpsOps(val file: Future[Option[(Array[Byte],String)]]) extends AnyVal{
    def response(notFound: Future[Response]) = file flatMap  {
      case Some((a,ct)) =>
        val response = Response()
        response.statusCode = 200
        response.write(a)
        response.contentType = ct
        Future(response)

      case None => notFound

    }
  }

}