package io.rout.routing

import com.twitter.finagle.{Filter, Service}
import com.twitter.finagle.http.{Method, Request, Response}
import com.twitter.io.Charsets
import com.twitter.util.Future
import io.rout.{Output, ReqExt, ReqRead, ToResponse}
import io.rout.path.Path
import shapeless.Lazy

case class PathToService[A](m: Method,path: Path){

  //object filter { def apply[B] = new filter[B] {} }

  private def mkResponse(response: Response) = {
    response.charset = Charsets.Utf8.toString
    Future.value(response)
  }

  private def reqFn(method: Method,path: Path): Request => Boolean = (request: Request) =>
    if(method == request.method && Path.compare(path,request.path)) true else false

  def filter[B] = new filter[B] {}

  trait filter[B]{

    def apply[AA,C,CT <:String](rr: ReqRead[Future[AA]])(service: (B,AA) => Output[C])(
      implicit tr: ToResponse.Aux[Output[C],CT]) = {
      (filter: Filter[Request,Response,ReqExt[B],Response]) => RequestToService(
        reqFn(m, path), Service.mk(filter.andThen(rr.toFilter2[B]) andThen service.tupled.andThen(x=> mkResponse(tr(x)))))
    }

    def apply[AA,C,CT <:String](rr: Lazy[ReqRead[AA]])(service: (B,AA) => Output[C])(
      implicit tr: ToResponse.Aux[Output[C],CT]) = {
        (filter: Filter[Request,Response,ReqExt[B],Response]) => RequestToService(
          reqFn(m, path), Service.mk(filter.andThen(rr.value.toFilter2[B]) andThen service.tupled.andThen(x=> mkResponse(tr(x)))))
    }

    def apply[AA,CT <:String](service:  B => Output[AA])(
      implicit tr: ToResponse.Aux[Output[AA],CT] ) =
      (filter: Filter[Request,Response,ReqExt[B],Response]) => RequestToService(
        reqFn(m,path),Service.mk(filter.andThen(aa=> service.andThen(x=> mkResponse(tr(x)))(aa.value))))

    def partial[AA,CT <:String](service:  PartialFunction[B,Output[AA]])(
      implicit tr: ToResponse.Aux[Output[AA],CT] ) =
      (filter: Filter[Request,Response,ReqExt[B],Response]) => RequestToService(
        reqFn(m,path),Service.mk(filter.andThen(aa=> service.andThen(x=> mkResponse(tr(x)))(aa.value))))
  }

  def service(service:  Request => Future[Response]) =
    RequestToService(reqFn(m,path),Service.mk(service))

  def service(rr: Lazy[ReqRead[A]])(service: A => Future[Response]) =
    RequestToService(reqFn(m,path),Service.mk(rr.value.toFilter andThen service))

  def service(rr: ReqRead[Future[A]])(service: A => Future[Response]) =
    RequestToService(reqFn(m,path),Service.mk(rr.toFilter andThen service))

  def apply[B,CT <:String](service:  Request => Output[B])(implicit tr: ToResponse.Aux[Output[B],CT] ) =
    RequestToService(reqFn(m,path),Service.mk(service.andThen(x=> mkResponse(tr(x)))))

  def apply(filter: Filter[Request,Response,A,Response])(service: A => Future[Response]) =
    RequestToService(reqFn(m,path),Service.mk(filter andThen service))

  def apply[B,CT <:String](rr: ReqRead[Future[A]])(service: A => Output[B])
                        (implicit tr: ToResponse.Aux[Output[B],CT] ) = {
    RequestToService(reqFn(m,path),Service.mk(rr.toFilter andThen service.andThen(x=> mkResponse(tr(x)))))
  }

  def apply[B,CT <:String](rr: Lazy[ReqRead[A]])(service: A => Output[B])
                        (implicit tr: ToResponse.Aux[Output[B],CT] ) = {
    RequestToService(reqFn(m,path),Service.mk(rr.value.toFilter andThen service.andThen(x=> mkResponse(tr(x)))))
  }

}

case class PathToServiceOption[A,B](m: Method,path: Path => Option[(B,Path)]) {

  //object filter{ def apply[AA] = new filter[AA] {} }

  private def mkResponse(response: Response) = {
    response.charset = Charsets.Utf8.toString
    Future.value(response)
  }

  private def reqFn(method: Method, path: Path => Option[B]): Request => Boolean = (request: Request) =>
    if (method == request.method) path(Path(request.path)) match {
      case Some(_) => true
      case None => false
    } else false

  def filter[AA] = new filter[AA] {}

  trait filter[AA]{

    def apply[CC,D,CT <:String](rr: ReqRead[Future[CC]])(service: (AA,B,CC) => Output[D])(
      implicit tr: ToResponse.Aux[Output[D],CT]) = {
      (filter: Filter[Request,Response,ReqExt[AA],Response]) => RequestToService(
        reqFn(m, Path(path)), Service.mk(filter.andThen(rr.toFilter.joinPath2[B,AA](path)).andThen(abo =>
          abo match {
            case None => NotFoundException.Future
            case Some((a,c,b)) => mkResponse(tr(service(a,b,c)))
          }
        )))
    }

    def apply[CC,D,CT <:String](rr: Lazy[ReqRead[CC]])(service: (AA,B,CC) => Output[D])(
      implicit tr: ToResponse.Aux[Output[D],CT]) = {
        (filter: Filter[Request,Response,ReqExt[AA],Response]) => RequestToService(
          reqFn(m, Path(path)), Service.mk(filter.andThen(rr.value.toFilter.joinPath2[B,AA](path)).andThen(abo =>
            abo match {
              case None => NotFoundException.Future
              case Some((a,c,b)) => mkResponse(tr(service(a,b,c)))
            }
          )))
    }

    def apply[C, CT <: String](service: (AA,B) => Output[C])(
      implicit tr: ToResponse.Aux[Output[C], CT]) = {
        (filter: Filter[Request,Response,ReqExt[AA],Response]) => RequestToService(
          reqFn(m, Path(path)), Service.mk(filter.joinPath[B](path).andThen(abo =>
           abo match {
              case None => NotFoundException.Future
             case Some((reqExt, path_)) => mkResponse(tr(service(reqExt.value,path_)))
           }
        )))
    }
  }

  def service(service: Request => Future[Response]) =
    RequestToService(reqFn(m, Path(path)), Service.mk(service))

  def fromPath(service: B => Future[Response]) =
    RequestToService(reqFn(m, Path(path)), Service.mk(path.toFilter.andThen(abo => abo match {
      case None => NotFoundException.Future
      case Some(b) => service(b._1)
    })))

  def service(rr: Lazy[ReqRead[A]])(service: (B,A) => Future[Response]) =
    RequestToService(reqFn(m, Path(path)), Service.mk(rr.value.toFilter.joinPath[B](path).andThen(abo => abo match {
      case None => NotFoundException.Future
      case Some((a,b)) => service(b,a)
    })))

  def service(rr: ReqRead[Future[A]])(service: (B,A) => Future[Response]) =
    RequestToService(reqFn(m, Path(path)), Service.mk(rr.toFilter.joinPath[B](path).andThen(abo => abo match {
      case None => NotFoundException.Future
      case Some((a,b)) => service(b,a)
    })))

  def apply[C,CT <:String](service: B => Output[C])
                          (implicit tr: ToResponse.Aux[Output[C],CT] ) = {
    RequestToService(reqFn(m, Path(path)), Service.mk(path.toFilter.andThen(abo =>
      abo match {
        case None => NotFoundException.Future
        case Some((b,path_)) => mkResponse(tr(service(b)))
      }
    )))
  }

  def apply(filter: Filter[Request, Response, A, Response])(service: (B,A) => Future[Response]) =
    RequestToService(reqFn(m, Path(path)), Service.mk(filter.joinPath[B](path).andThen(abo =>
      abo match {
      case None => NotFoundException.Future
      case Some((a,b)) => service(b,a)
      }
    )))

  def apply[C,CT <:String](rr: Lazy[ReqRead[A]])(service: (B,A)=> Output[C])
                        (implicit tr: ToResponse.Aux[Output[C], CT]): RequestToService = {
    RequestToService(reqFn(m, Path(path)), Service.mk(rr.value.toFilter.joinPath[B](path).andThen(abo =>
      abo match {
      case None => NotFoundException.Future
      case Some((a,b)) => mkResponse(tr(service(b,a)))
      }
    )))
  }

  def apply[C,CT <:String](rr: ReqRead[Future[A]])(service: (B,A) => Output[C])
                          (implicit tr: ToResponse.Aux[Output[C], CT]): RequestToService = {
    RequestToService(reqFn(m, Path(path)), Service.mk(rr.toFilter.joinPath[B](path).andThen(abo =>
      abo match {
      case None => NotFoundException.Future
      case Some((a,b)) => mkResponse(tr(service(b,a)))
      }
    )))
  }
}

case class RequestToService(request: Request => Boolean,service: Service[Request,Response]) extends  Routable{ self =>

  val routes = Seq(self)

  def filter(filter: Filter[Request,Response,Request,Response]) =
    copy(service = filter andThen service)

  def withFilter(filter: Filter[Request,Response,Request,Response]) =
    copy(service = filter andThen service)
}
