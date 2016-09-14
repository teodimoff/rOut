package io.rout.routing

import com.twitter.finagle.http._
import com.twitter.logging.{Level, Logger}
import io.rout.path.{MatchPath, Path}

trait Rout {

  val log = Logger()

  def mkRoutes(seq: Seq[RequestToService]) =
    Routing(seq)

  def mkRoutable(seq: Seq[Routable]) =
    Routing(seq.flatMap(_.routes))

  def path[A] = MatchPath[A] _

  def Match[A] = MatchPath[A] _

  val Root = io.rout.path.Root

  def get[CT <:String](path: Path) = PathToService[CT](Method.Get,path)

  def get[B,CT <:String](path: Path => Option[(B,Path)]) =
    PathToServiceOption[B,CT](Method.Get,path)

  def post[CT <:String](path: Path) =
    PathToService[CT](Method.Post,path)

  def post[B,CT <:String](path: Path => Option[(B,Path)]) =
    PathToServiceOption[B,CT](Method.Post,path)

  def patch[CT <:String](path: Path) =
    PathToService[CT](Method.Patch,path)

  def patch[B,CT <:String](path: Path => Option[(B,Path)]) =
    PathToServiceOption[B,CT](Method.Patch,path)

  def delete[CT <:String](path: Path) =
    PathToService[CT](Method.Delete,path)

  def delete[B,CT <:String](path: Path => Option[(B,Path)]) =
    PathToServiceOption[B,CT](Method.Delete,path)

  def put[CT <:String](path: Path) =
    PathToService[CT](Method.Put,path)

  def put[B,CT <:String](path: Path => Option[(B,Path)]) =
    PathToServiceOption[B,CT](Method.Put,path)

  def connect[CT <:String](path: Path) =
    PathToService[CT](Method.Connect,path)

  def connect[B,CT <:String](path: Path => Option[(B,Path)]) =
    PathToServiceOption[B,CT](Method.Connect,path)

  def trace[CT <:String](path: Path) =
    PathToService[CT](Method.Trace,path)

  def trace[B,CT <:String](path: Path => Option[(B,Path)]) =
    PathToServiceOption[B,CT](Method.Trace,path)

  def options[CT <:String](path: Path) =
    PathToService[CT](Method.Options,path)

  def options[B,CT <:String](path: Path => Option[(B,Path)]) =
    PathToServiceOption[B,CT](Method.Options,path)

}

/*
object Rout {

  def apply(seq: RequestToService*) = mkRoutes(seq: _*)

  def apply(seq: Routable*) = mkRoutable(seq: _*)

}
 */
