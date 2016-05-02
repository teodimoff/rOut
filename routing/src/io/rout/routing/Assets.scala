package io.rout.routing

import com.twitter.finagle.http.Response
import io.rout.path.Path
import com.twitter.util.{Future}
import io.rout.file.{FileOps, FileOpsResponse}


case class Assets(assetPrefix: String, file: FileOps) extends Rout {
  import Assets._
  private val drop = Path(assetPrefix).toList.size
  private val FNF = Future(notFound)

  def asset1 =  get(Root / assetPrefix / path[String]).service(r =>
    file.read(Path(r.path).drop(drop).toString).response(FNF))

  def asset2 = get(Root / assetPrefix / path[String] / path[String]).service(r =>
    file.read(Path(r.path).drop(drop).toString).response(FNF))

  def asset3 =get(Root / assetPrefix / path[String] / path[String] / path[String]).service(r =>
    file.read(Path(r.path).drop(drop).toString).response(FNF))

  def asset4 = get(Root / assetPrefix / path[String] / path[String] / path[String] / path[String]).service(r =>
    file.read(Path(r.path).drop(drop).toString).response(FNF))

  def asset5 = get(Root / assetPrefix / path[String] / path[String] / path[String] / path[String] / path[String]).service(r =>
    file.read(Path(r.path).drop(drop).toString).response(FNF))

  //use path param to read file from local filesystem eq :8080/local/pic.jpg?path=/home/teodimoff/
  private val localAsset = get(Root / "local" / path[String]).service(param("path"))((path,localRoot) =>
    file.static(localRoot + "/" + path,Future(notFound)))

  private val seeAllCached = get(Root / "see").service(request => file.seeAll())

  def addDebug() = Seq(localAsset,seeAllCached)

  def assetDepth(depth: Int) = depth match {
    case 1 => Seq(asset1)
    case 2 => Seq(asset1,asset2)
    case 3 => Seq(asset1,asset2,asset3)
    case 4 => Seq(asset1,asset2,asset3 ,asset4)
    case 5 => Seq(asset1,asset2,asset3 ,asset4 ,asset5)
  }
}
object Assets {

  implicit class FileOpsOps(val file: Future[Option[FileOpsResponse]]) extends AnyVal{
    def response(notFound: Future[Response]) = file flatMap  {
      case Some(resp)=>
        val response = Response()
        response.statusCode = 200
        response.content = resp.file
        response.contentType = resp.contentType
        Future(response)

      case None => notFound

    }
  }

}