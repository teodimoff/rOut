package io.rout.routing

import com.twitter.finagle.http._
import com.twitter.logging.{Level, Logger}
import com.twitter.util.Future
import io.rout.file.FileOps
import io.rout.path.{MatchPath, Path, Root => RootPath}

trait Rout {

  val log = Logger()
  val notFound = Response(Status.NotFound)

  def apply(seq: Seq[RequestToService],
            futureNotFound: Future[Response] = Future.value(notFound)
           ) = mkRoutes(seq,futureNotFound)

  def mkRoutes(seq: Seq[RequestToService], fnd: Future[Response] = Future.value(notFound)) =
    new Routing(seq,fnd)

    def mkRoutable(seq: Seq[Routable],
                 futureNotFound: Future[Response] = Future.value(notFound)) = {
      log.log(Level.INFO,"\n" + seq.map(r=> r.routes.map(x=>
        s"""\n${r.className}""").mkString("")).mkString("\n"))
      new Routing(seq.flatMap(_.routes),futureNotFound)
    }

  def path[A] = MatchPath[A] _

  def Match[A] = MatchPath[A] _

  val Root = RootPath

  def get[A](path: Path) = PathToService[A](Method.Get,path)

  def get[A,B](path: Path => Option[(B,Path)]) =
    PathToServiceOption[A,B](Method.Get,path,Future.value(notFound))

  def post[A](path: Path) =
    PathToService[A](Method.Post,path)

  def post[A,B](path: Path => Option[(B,Path)]) =
    PathToServiceOption[A,B](Method.Post,path,Future.value(notFound))

  def patch[A](path: Path) =
    PathToService[A](Method.Patch,path)

  def patch[A,B](path: Path => Option[(B,Path)]) =
    PathToServiceOption[A,B](Method.Patch,path,Future.value(notFound))

  def delete[A](path: Path) =
    PathToService[A](Method.Delete,path)

  def delete[A,B](path: Path => Option[(B,Path)]) =
    PathToServiceOption[A,B](Method.Delete,path,Future.value(notFound))

  def put[A](path: Path) =
    PathToService[A](Method.Put,path)

  def put[A,B](path: Path => Option[(B,Path)]) =
    PathToServiceOption[A,B](Method.Put,path,Future.value(notFound))

  def connect[A](path: Path) =
    PathToService[A](Method.Connect,path)

  def connect[A,B](path: Path => Option[(B,Path)]) =
    PathToServiceOption[A,B](Method.Connect,path,Future.value(notFound))

  def trace[A](path: Path) =
    PathToService[A](Method.Trace,path)

  def trace[A,B](path: Path => Option[(B,Path)]) =
    PathToServiceOption[A,B](Method.Trace,path,Future.value(notFound))

  def options[A](path: Path) =
    PathToService[A](Method.Options,path)

  def options[A,B](path: Path => Option[(B,Path)]) =
    PathToServiceOption[A,B](Method.Options,path,Future.value(notFound))

  //serve static content eg. /css/bootstrap.css
  def static(staticPath: String)(path: Path) =
    get(path).service(req => FileOps.static(staticPath,Future(notFound)))

}


