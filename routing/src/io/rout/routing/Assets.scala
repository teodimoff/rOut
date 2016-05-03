package io.rout.routing

import com.twitter.finagle.http.Response
import io.rout.path.Path
import com.twitter.util.{Future, StorageUnit}
import io.rout.file.{FileOps, FileOpsResponse}

//maybe refactor with configurable ?
case class Assets(file: FileOps, seq: Seq[RequestToService] = Nil)  {
  import Assets._
  private val FNF = Future(notFound)

  def asset1(assetPrefix: String) = copy(file,seq :+ get(Root / assetPrefix / path[String]).service(r =>
    file.read(Path(r.path).drop(Path(assetPrefix).depth).toString).response(FNF)))

  def asset2(assetPrefix: String) =  copy(file,seq :+get(Root / assetPrefix / path[String] / path[String]).service(r =>
    file.read(Path(r.path).drop(Path(assetPrefix).depth).toString).response(FNF)))

  def asset3(assetPrefix: String) =  copy(file,seq :+get(Root / assetPrefix / path[String] / path[String] / path[String]).service(r =>
    file.read(Path(r.path).drop(Path(assetPrefix).depth).toString).response(FNF)))

  def asset4(assetPrefix: String) =  copy(file,seq :+get(Root / assetPrefix / path[String] / path[String] / path[String] / path[String]).service(r =>
    file.read(Path(r.path).drop(Path(assetPrefix).depth).toString).response(FNF)))

  def asset5(assetPrefix: String) =  copy(file,seq :+get(Root / assetPrefix / path[String] / path[String] / path[String] / path[String] / path[String]).service(r =>
    file.read(Path(r.path).drop(Path(assetPrefix).depth).toString).response(FNF)))

  def debug: Assets = copy(file,seq ++ Assets.addDebug(file))

  def :+(a: Seq[Assets]) = copy(file,seq ++ a.flatMap(_.seq))

  def +:(add: Seq[RequestToService]) = copy(file, add ++ seq)

  def done = Routing(seq,FNF)

  def asset(assetPrefix: String, depth: Int) = depth match {
    case 1 =>
      copy(file,seq ++ Seq(asset1(assetPrefix)).flatMap(_.seq))
    case 2 =>
      copy(file,seq ++ Seq(asset1(assetPrefix),asset2(assetPrefix)).flatMap(_.seq))
    case 3 =>
      copy(file,seq ++ Seq(asset1(assetPrefix),asset2(assetPrefix),asset3(assetPrefix)).flatMap(_.seq))
    case 4 =>
      copy(file,seq ++ Seq(asset1(assetPrefix),asset2(assetPrefix),asset3(assetPrefix),asset4(assetPrefix)).flatMap(_.seq))
    case 5 =>
      copy(file,seq ++ Seq(asset1(assetPrefix),asset2(assetPrefix),asset3(assetPrefix),asset4(assetPrefix),asset5(assetPrefix)).flatMap(_.seq))
  }
}
object Assets  {
  //use path param to read file from local filesystem eq :8080/local/pic.jpg?path=/home/teodimoff/
  def addDebug(file: FileOps) = Seq(
    get(Root / "see").service(request => file.seeAll()),
    get(Root / "local" / path[String]).service(param("path"))((path, localRoot) =>
      file.static(localRoot + "/" + path, Future(notFound))))

  def apply(size: StorageUnit) = new Assets(FileOps(size))

  implicit class AssetOps(val assets: Assets) extends AnyVal {
    def add(routing: Routing) = routing :+ assets.seq
  }

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