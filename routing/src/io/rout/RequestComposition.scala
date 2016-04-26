package io.rout.routing

import com.twitter.finagle.{Filter, Service}
import com.twitter.finagle.http.{Method, Request, Response}
import com.twitter.util.Future
import io.rout.{Output, ReqRead, RequestExt, ToResponse}
import io.rout.path.Path
import shapeless.Lazy

case class PathToService[A](m: Method,path: Path){

  private def reqFn(method: Method,path: Path): Request => Boolean = (request: Request) =>
    if(method == request.method && Path.compare(path,request.path))true else false

  def service(service:  Request => Future[Response]): RequestToService =
    RequestToService(reqFn(m,path),Service.mk(service))

  def service(rr: Lazy[ReqRead[A]])(service: A => Future[Response]): RequestToService =
    RequestToService(reqFn(m,path),Service.mk(rr.value.toFilter andThen service))

  def service(rr: ReqRead[Future[A]])(service: A => Future[Response]): RequestToService =
    RequestToService(reqFn(m,path),Service.mk(rr.toFilter andThen service))

  def apply[B,CT <:String](service:  Request => Output[B]) (implicit tr: ToResponse.Aux[Output[B],CT] ): RequestToService =
    RequestToService(reqFn(m,path),Service.mk(service.andThen(x=> Future(tr(x)))))

  def apply(filter: Filter[Request,Response,A,Response])(service: A => Future[Response]): RequestToService =
    RequestToService(reqFn(m,path),Service.mk(filter andThen service))

  def apply[B,CT <:String](rr: ReqRead[Future[A]])(service: A => Output[B])
                        (implicit tr: ToResponse.Aux[Output[B],CT] ): RequestToService = {
    RequestToService(reqFn(m,path),Service.mk(rr.toFilter andThen service.andThen(x=> Future(tr(x)))))
  }

  def apply[B,CT <:String](rr: Lazy[ReqRead[A]])(service: A => Output[B])
                        (implicit tr: ToResponse.Aux[Output[B],CT] ): RequestToService = {
    RequestToService(reqFn(m,path),Service.mk(rr.value.toFilter andThen service.andThen(x=> Future(tr(x)))))
  }

}

case class PathToServiceOption[A,B](m: Method,path: Path => Option[(B,Path)],notFound: Future[Response]) {

  private def reqFn(method: Method, path: Path => Option[B]): Request => Boolean = (request: Request) =>
    if (method == request.method) path(Path(request.path)) match {
      case Some(_) => true
      case None => false
    } else false

  def service(service: Request => Future[Response]): RequestToService =
    RequestToService(reqFn(m, Path(path)), Service.mk(service))


  def service(rr: Lazy[ReqRead[A]])(service: (B,A) => Future[Response]) =
    RequestToService(reqFn(m, Path(path)), Service.mk(rr.value.toFilter.joinPath[B](path).andThen(abo => abo match {
      case None => notFound
      case Some(ab) => service(ab._2, ab._1)
    })))

  def service(rr: ReqRead[Future[A]])(service: (B,A) => Future[Response]) =
    RequestToService(reqFn(m, Path(path)), Service.mk(rr.toFilter.joinPath[B](path).andThen(abo => abo match {
      case None => notFound
      case Some(ab) => service(ab._2, ab._1)
    })))


  def apply[C,CT <:String](service: B => Output[C])
                          (implicit tr: ToResponse.Aux[Output[C],CT] ): RequestToService = {
    RequestToService(reqFn(m, Path(path)), Service.mk(path.toFilter.andThen(abo =>
      abo match {
        case None => notFound
        case Some(ab) => Future(tr(service(ab._1)))
      }
    )))
  }

  def apply(filter: Filter[Request, Response, A, Response])(service: (A, B) => Future[Response]) =
    RequestToService(reqFn(m, Path(path)), Service.mk(filter.joinPath[B](path).andThen(abo =>
      abo match {
      case None => notFound
      case Some(ab) => service(ab._1, ab._2)
      }
    )))

  def apply[C,CT <:String](rr: Lazy[ReqRead[A]])(service: (B,A)=> Output[C])
                        (implicit tr: ToResponse.Aux[Output[C], CT]): RequestToService = {
    RequestToService(reqFn(m, Path(path)), Service.mk(rr.value.toFilter.joinPath[B](path).andThen(abo =>
      abo match {
      case None => notFound
      case Some(ab) => Future(tr(service(ab._2, ab._1)))
      }
    )))
  }

  def apply[C,CT <:String](rr: ReqRead[Future[A]])(service: (B,A) => Output[C])
                          (implicit tr: ToResponse.Aux[Output[C], CT]): RequestToService = {
    RequestToService(reqFn(m, Path(path)), Service.mk(rr.toFilter.joinPath[B](path).andThen(abo =>
      abo match {
      case None => notFound
      case Some(ab) => Future(tr(service(ab._2, ab._1)))
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
