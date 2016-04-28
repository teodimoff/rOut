package io.rout.routing

import com.twitter.finagle.{Filter, Service}
import com.twitter.finagle.http.{Method, Request, Response}
import com.twitter.util.Future
import io.rout.{Output, ReqExt, ReqRead, ToResponse}
import io.rout.path.Path
import shapeless.Lazy

case class PathToService[A](m: Method,path: Path){

  case class filter[B,AA](rr: ReqRead[AA]){
    def apply[C,CT <:String](service: (B,AA) => Output[C])
                            (implicit tr: ToResponse.Aux[Output[C],CT]) = {
      (filter: Filter[Request,Response,ReqExt[B],Response]) =>
        RequestToService(reqFn(m, path),
          Service.mk(filter.andThen(rr.toFilter2[B]) andThen service.tupled.andThen(x=> Future(tr(x)))))
    }
  }

  private def reqFn(method: Method,path: Path): Request => Boolean = (request: Request) =>
    if(method == request.method && Path.compare(path,request.path)) true else false

  def service(service:  Request => Future[Response]) =
    RequestToService(reqFn(m,path),Service.mk(service))

  def service(rr: Lazy[ReqRead[A]])(service: A => Future[Response]) =
    RequestToService(reqFn(m,path),Service.mk(rr.value.toFilter andThen service))

  def service(rr: ReqRead[Future[A]])(service: A => Future[Response]) =
    RequestToService(reqFn(m,path),Service.mk(rr.toFilter andThen service))

  def apply[B,CT <:String](service:  Request => Output[B])(implicit tr: ToResponse.Aux[Output[B],CT] ) =
    RequestToService(reqFn(m,path),Service.mk(service.andThen(x=> Future(tr(x)))))

  def apply(filter: Filter[Request,Response,A,Response])(service: A => Future[Response]) =
    RequestToService(reqFn(m,path),Service.mk(filter andThen service))

  def apply[B,CT <:String](rr: ReqRead[Future[A]])(service: A => Output[B])
                        (implicit tr: ToResponse.Aux[Output[B],CT] ) = {
    RequestToService(reqFn(m,path),Service.mk(rr.toFilter andThen service.andThen(x=> Future(tr(x)))))
  }

  def apply[B,CT <:String](rr: Lazy[ReqRead[A]])(service: A => Output[B])
                        (implicit tr: ToResponse.Aux[Output[B],CT] ) = {
    RequestToService(reqFn(m,path),Service.mk(rr.value.toFilter andThen service.andThen(x=> Future(tr(x)))))
  }

}

case class PathToServiceOption[A,B](m: Method,path: Path => Option[(B,Path)],notFound: Future[Response]) {

  private def reqFn(method: Method, path: Path => Option[B]): Request => Boolean = (request: Request) =>
    if (method == request.method) path(Path(request.path)) match {
      case Some(_) => true
      case None => false
    } else false

  def service(service: Request => Future[Response]) =
    RequestToService(reqFn(m, Path(path)), Service.mk(service))

  def service(rr: Lazy[ReqRead[A]])(service: (B,A) => Future[Response]) =
    RequestToService(reqFn(m, Path(path)), Service.mk(rr.value.toFilter.joinPath[B](path).andThen(abo => abo match {
      case None => notFound
      case Some((a,b)) => service(b,a)
    })))

  def service(rr: ReqRead[Future[A]])(service: (B,A) => Future[Response]) =
    RequestToService(reqFn(m, Path(path)), Service.mk(rr.toFilter.joinPath[B](path).andThen(abo => abo match {
      case None => notFound
      case Some((a,b)) => service(b,a)
    })))

  case class filter[AA,CC](rr: ReqRead[CC]){
    def apply[D,CT <:String](service: (AA,B,CC) => Output[D])
                              (implicit tr: ToResponse.Aux[Output[D],CT]) = {
      (filter: Filter[Request,Response,ReqExt[AA],Response]) =>
        RequestToService(reqFn(m, Path(path)),
          Service.mk(filter.andThen(rr.toFilter.joinPath2[B,AA](path)).andThen(abo =>
            abo match {
              case None => notFound
              case Some((a,c,b)) => Future(tr(service(a,b,c)))
            }
          )))
    }
  }

  def apply[C,CT <:String](service: B => Output[C])
                          (implicit tr: ToResponse.Aux[Output[C],CT] ) = {
    RequestToService(reqFn(m, Path(path)), Service.mk(path.toFilter.andThen(abo =>
      abo match {
        case None => notFound
        case Some((b,path_)) => Future(tr(service(b)))
      }
    )))
  }

  def apply(filter: Filter[Request, Response, A, Response])(service: (B,A) => Future[Response]) =
    RequestToService(reqFn(m, Path(path)), Service.mk(filter.joinPath[B](path).andThen(abo =>
      abo match {
      case None => notFound
      case Some((a,b)) => service(b,a)
      }
    )))

  def apply[C,CT <:String](rr: Lazy[ReqRead[A]])(service: (B,A)=> Output[C])
                        (implicit tr: ToResponse.Aux[Output[C], CT]): RequestToService = {
    RequestToService(reqFn(m, Path(path)), Service.mk(rr.value.toFilter.joinPath[B](path).andThen(abo =>
      abo match {
      case None => notFound
      case Some((a,b)) => Future(tr(service(b,a)))
      }
    )))
  }

  def apply[C,CT <:String](rr: ReqRead[Future[A]])(service: (B,A) => Output[C])
                          (implicit tr: ToResponse.Aux[Output[C], CT]): RequestToService = {
    RequestToService(reqFn(m, Path(path)), Service.mk(rr.toFilter.joinPath[B](path).andThen(abo =>
      abo match {
      case None => notFound
      case Some((a,b)) => Future(tr(service(b,a)))
      }
    )))
  }
}

case class RequestToService(request: Request => Boolean,service: Service[Request,Response]) {
  def filter(filter: Filter[Request,Response,Request,Response]) =
    copy(service = filter andThen service)

  def withFilter(filter: Filter[Request,Response,Request,Response]) =
    copy(service = filter andThen service)
}
