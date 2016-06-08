package io.rout.routing

import com.twitter.finagle.http._
import com.twitter.logging.{Level, Logger}
import io.rout.path.{MatchPath, Path}

trait Rout {

  val log = Logger()

  def apply(seq: Seq[RequestToService]) = mkRoutes(seq)

  def mkRoutes(seq: Seq[RequestToService]) =
    Routing(seq)

  def mkRoutable(seq: Seq[Routable]) =
    Routing(seq.flatMap(_.routes))

  def path[A] = MatchPath[A] _

  def Match[A] = MatchPath[A] _

  val Root = io.rout.path.Root

  def get[A](path: Path) = PathToService[A](Method.Get,path)

  def get[A,B](path: Path => Option[(B,Path)]) =
    PathToServiceOption[A,B](Method.Get,path)

  def post[A](path: Path) =
    PathToService[A](Method.Post,path)

  def post[A,B](path: Path => Option[(B,Path)]) =
    PathToServiceOption[A,B](Method.Post,path)

  def patch[A](path: Path) =
    PathToService[A](Method.Patch,path)

  def patch[A,B](path: Path => Option[(B,Path)]) =
    PathToServiceOption[A,B](Method.Patch,path)

  def delete[A](path: Path) =
    PathToService[A](Method.Delete,path)

  def delete[A,B](path: Path => Option[(B,Path)]) =
    PathToServiceOption[A,B](Method.Delete,path)

  def put[A](path: Path) =
    PathToService[A](Method.Put,path)

  def put[A,B](path: Path => Option[(B,Path)]) =
    PathToServiceOption[A,B](Method.Put,path)

  def connect[A](path: Path) =
    PathToService[A](Method.Connect,path)

  def connect[A,B](path: Path => Option[(B,Path)]) =
    PathToServiceOption[A,B](Method.Connect,path)

  def trace[A](path: Path) =
    PathToService[A](Method.Trace,path)

  def trace[A,B](path: Path => Option[(B,Path)]) =
    PathToServiceOption[A,B](Method.Trace,path)

  def options[A](path: Path) =
    PathToService[A](Method.Options,path)

  def options[A,B](path: Path => Option[(B,Path)]) =
    PathToServiceOption[A,B](Method.Options,path)

}

