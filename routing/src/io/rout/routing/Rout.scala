package io.rout.routing

import com.twitter.finagle.http._
import com.twitter.logging.{Level, Logger}
import com.twitter.util.Future
import io.rout.path.{MatchPath, Path, Root => RootPath}

trait Rout {

  val log = Logger()
  val notFound = Response(Status.NotFound)
  val FNF = Future(notFound)

  def apply(seq: Seq[RequestToService],
            FNF: Future[Response] = FNF
           ) = mkRoutes(seq,FNF)

  def mkRoutes(seq: Seq[RequestToService], FNF: Future[Response] =FNF) =
     Routing(seq,FNF)

  def mkRoutable(seq: Seq[Routable], FNF: Future[Response] =FNF) =
     Routing(seq.flatMap(_.routes),FNF)

  def path[A] = MatchPath[A] _

  def Match[A] = MatchPath[A] _

  val Root = RootPath

  def get[A](path: Path) = PathToService[A](Method.Get,path)

  def get[A,B](path: Path => Option[(B,Path)]) =
    PathToServiceOption[A,B](Method.Get,path,FNF)

  def post[A](path: Path) =
    PathToService[A](Method.Post,path)

  def post[A,B](path: Path => Option[(B,Path)]) =
    PathToServiceOption[A,B](Method.Post,path,FNF)

  def patch[A](path: Path) =
    PathToService[A](Method.Patch,path)

  def patch[A,B](path: Path => Option[(B,Path)]) =
    PathToServiceOption[A,B](Method.Patch,path,FNF)

  def delete[A](path: Path) =
    PathToService[A](Method.Delete,path)

  def delete[A,B](path: Path => Option[(B,Path)]) =
    PathToServiceOption[A,B](Method.Delete,path,FNF)

  def put[A](path: Path) =
    PathToService[A](Method.Put,path)

  def put[A,B](path: Path => Option[(B,Path)]) =
    PathToServiceOption[A,B](Method.Put,path,FNF)

  def connect[A](path: Path) =
    PathToService[A](Method.Connect,path)

  def connect[A,B](path: Path => Option[(B,Path)]) =
    PathToServiceOption[A,B](Method.Connect,path,FNF)

  def trace[A](path: Path) =
    PathToService[A](Method.Trace,path)

  def trace[A,B](path: Path => Option[(B,Path)]) =
    PathToServiceOption[A,B](Method.Trace,path,FNF)

  def options[A](path: Path) =
    PathToService[A](Method.Options,path)

  def options[A,B](path: Path => Option[(B,Path)]) =
    PathToServiceOption[A,B](Method.Options,path,FNF)

}

