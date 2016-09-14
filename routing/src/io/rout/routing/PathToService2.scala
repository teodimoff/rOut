package io.rout.routing

import com.twitter.finagle.{Filter, Service}
import com.twitter.finagle.http.{Method, Request, Response}
import com.twitter.io.Charsets
import com.twitter.util.Future
import io.rout.{Output, ReqReadExt, ReqRead, ToResponse}
import io.rout.path.Path
import shapeless.Lazy

//todo clean all this mess
case class PathToServiceOption[B,CT <:String](m: Method,path: Path => Option[(B,Path)]) {

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

    def sync[CC,D](rr: ReqRead[Future[CC]])(service: (AA,B,CC) => Output[D])(
      implicit tr: ToResponse.Aux[Output[D],CT]) = {
      FilterRequestToService((filter: Filter[Request,Response,ReqReadExt[AA],Response]) => RequestToService(
        reqFn(m, Path(path)), Service.mk(filter.andThen(rr.toFilter.joinPath2[B,AA](path)).andThen(abo =>
          abo match {
            case None => NotFoundException.Future
            case Some((a,c,b)) => mkResponse(tr(service(a,b,c)))
          }
        ))))
    }

    def sync[CC,D](rr: Lazy[ReqRead[CC]])(service: (AA,B,CC) => Output[D])(
      implicit tr: ToResponse.Aux[Output[D],CT]) = {
      FilterRequestToService((filter: Filter[Request,Response,ReqReadExt[AA],Response]) => RequestToService(
        reqFn(m, Path(path)), Service.mk(filter.andThen(rr.value.toFilter.joinPath2[B,AA](path)).andThen(abo =>
          abo match {
            case None => NotFoundException.Future
            case Some((a,c,b)) => mkResponse(tr(service(a,b,c)))
          }
        ))))
    }

    def sync[C](service: (AA,B) => Output[C])(
      implicit tr: ToResponse.Aux[Output[C], CT]) = {
      FilterRequestToService((filter: Filter[Request,Response,ReqReadExt[AA],Response]) => RequestToService(
        reqFn(m, Path(path)), Service.mk(filter.joinPath[B](path).andThen(abo =>
          abo match {
            case None => NotFoundException.Future
            case Some((reqExt, path_)) => mkResponse(tr(service(reqExt.value,path_)))
          }
        ))))
    }

    def apply[CC,D](rr: ReqRead[Future[CC]])(service: (AA,B,CC) => Future[Output[D]])(
      implicit tr: ToResponse.Aux[Output[D],CT]) = {
      FilterRequestToService((filter: Filter[Request,Response,ReqReadExt[AA],Response]) => RequestToService(
        reqFn(m, Path(path)), Service.mk(filter.andThen(rr.toFilter.joinPath2[B,AA](path)).andThen(abo =>
          abo match {
            case None => NotFoundException.Future
            case Some((a,c,b)) => service(a,b,c).flatMap(r=> mkResponse(tr(r)))
          }
        ))))
    }

    def apply[CC,D](rr: Lazy[ReqRead[CC]])(service: (AA,B,CC) => Future[Output[D]])(
      implicit tr: ToResponse.Aux[Output[D],CT]) = {
      FilterRequestToService((filter: Filter[Request,Response,ReqReadExt[AA],Response]) => RequestToService(
        reqFn(m, Path(path)), Service.mk(filter.andThen(rr.value.toFilter.joinPath2[B,AA](path)).andThen(abo =>
          abo match {
            case None => NotFoundException.Future
            case Some((a,c,b)) => service(a,b,c).flatMap(r=> mkResponse(tr(r)))
          }
        ))))
    }

    def apply[C](service: (AA,B) => Future[Output[C]])(
      implicit tr: ToResponse.Aux[Output[C], CT]) = {
      FilterRequestToService((filter: Filter[Request,Response,ReqReadExt[AA],Response]) => RequestToService(
        reqFn(m, Path(path)), Service.mk(filter.joinPath[B](path).andThen(abo =>
          abo match {
            case None => NotFoundException.Future
            case Some((reqExt, path_)) => service(reqExt.value,path_).flatMap(r=> mkResponse(tr(r)))
          }
        ))))
    }
  }

  def service(service: Request => Future[Response]) =
    RequestToService(reqFn(m, Path(path)), Service.mk(service))

  def fromPath(service: B => Future[Response]) =
    RequestToService(reqFn(m, Path(path)), Service.mk(path.toFilter.andThen(abo => abo match {
      case None => NotFoundException.Future
      case Some(b) => service(b._1)
    })))

  def service[A](rr: Lazy[ReqRead[A]])(service: (B,A) => Future[Response]) =
    RequestToService(reqFn(m, Path(path)), Service.mk(rr.value.toFilter.joinPath[B](path).andThen(abo => abo match {
      case None => NotFoundException.Future
      case Some((a,b)) => service(b,a)
    })))

  def service[A](rr: ReqRead[Future[A]])(service: (B,A) => Future[Response]) =
    RequestToService(reqFn(m, Path(path)), Service.mk(rr.toFilter.joinPath[B](path).andThen(abo => abo match {
      case None => NotFoundException.Future
      case Some((a,b)) => service(b,a)
    })))

  def sync[C](service: B => Output[C])
                         (implicit tr: ToResponse.Aux[Output[C],CT] ) = {
    RequestToService(reqFn(m, Path(path)), Service.mk(path.toFilter.andThen(abo =>
      abo match {
        case None => NotFoundException.Future
        case Some((b,path_)) => mkResponse(tr(service(b)))
      }
    )))
  }

  def sync[A](filter: Filter[Request, Response, A, Response])(service: (B,A) => Future[Response]) =
    RequestToService(reqFn(m, Path(path)), Service.mk(filter.joinPath[B](path).andThen(abo =>
      abo match {
        case None => NotFoundException.Future
        case Some((a,b)) => service(b,a)
      }
    )))

  def sync[C,A](rr: Lazy[ReqRead[A]])(service: (B,A)=> Output[C])
                         (implicit tr: ToResponse.Aux[Output[C], CT]): RequestToService = {
    RequestToService(reqFn(m, Path(path)), Service.mk(rr.value.toFilter.joinPath[B](path).andThen(abo =>
      abo match {
        case None => NotFoundException.Future
        case Some((a,b)) => mkResponse(tr(service(b,a)))
      }
    )))
  }

  def sync[C,A](rr: ReqRead[Future[A]])(service: (B,A) => Output[C])
                         (implicit tr: ToResponse.Aux[Output[C], CT]): RequestToService = {
    RequestToService(reqFn(m, Path(path)), Service.mk(rr.toFilter.joinPath[B](path).andThen(abo =>
      abo match {
        case None => NotFoundException.Future
        case Some((a,b)) => mkResponse(tr(service(b,a)))
      }
    )))
  }


  def apply[C](service: B => Future[Output[C]])
                          (implicit tr: ToResponse.Aux[Output[C],CT] ) = {
    RequestToService(reqFn(m, Path(path)), Service.mk(path.toFilter.andThen(abo =>
      abo match {
        case None => NotFoundException.Future
        case Some((b,path_)) =>service(b).flatMap(r => mkResponse(tr(r)))
      }
    )))
  }

  def apply[C,A](rr: Lazy[ReqRead[A]])(service: (B,A)=> Future[Output[C]])
                          (implicit tr: ToResponse.Aux[Output[C], CT]): RequestToService = {
    RequestToService(reqFn(m, Path(path)), Service.mk(rr.value.toFilter.joinPath[B](path).andThen(abo =>
      abo match {
        case None => NotFoundException.Future
        case Some((a,b)) =>service(b,a).flatMap(r => mkResponse(tr(r)))
      }
    )))
  }

  def apply[C,A](rr: ReqRead[Future[A]])(service: (B,A) => Future[Output[C]])
                          (implicit tr: ToResponse.Aux[Output[C], CT]): RequestToService = {
    RequestToService(reqFn(m, Path(path)), Service.mk(rr.toFilter.joinPath[B](path).andThen(abo =>
      abo match {
        case None => NotFoundException.Future
        case Some((a,b)) => service(b,a).flatMap(r => mkResponse(tr(r)))
      }
    )))
  }

}

/*
package io.rout.routing

import com.twitter.finagle.{Filter, Service}
import com.twitter.finagle.http.{Method, Request, Response}
import com.twitter.io.Charsets
import com.twitter.util.Future
import io.rout.{Output, ReqExt, ReqRead, ToResponse}
import io.rout.path.Path
import shapeless.Lazy


case class PathToServiceOption[A,B](m: Method,path: Path => Option[(B,Path)]) {

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

    def sync[CC,D,CT <:String](rr: ReqRead[Future[CC]])(service: (AA,B,CC) => Output[D])(
      implicit tr: ToResponse.Aux[Output[D],CT]) = {
      FilterRequestToService((filter: Filter[Request,Response,ReqExt[AA],Response]) => RequestToService(
        reqFn(m, Path(path)), Service.mk(filter.andThen(rr.toFilter.joinPath2[B,AA](path)).andThen(abo =>
          abo match {
            case None => NotFoundException.Future
            case Some((a,c,b)) => mkResponse(tr(service(a,b,c)))
          }
        ))))
    }

    def sync[CC,D,CT <:String](rr: Lazy[ReqRead[CC]])(service: (AA,B,CC) => Output[D])(
      implicit tr: ToResponse.Aux[Output[D],CT]) = {
      FilterRequestToService((filter: Filter[Request,Response,ReqExt[AA],Response]) => RequestToService(
        reqFn(m, Path(path)), Service.mk(filter.andThen(rr.value.toFilter.joinPath2[B,AA](path)).andThen(abo =>
          abo match {
            case None => NotFoundException.Future
            case Some((a,c,b)) => mkResponse(tr(service(a,b,c)))
          }
        ))))
    }

    def sync[C, CT <: String](service: (AA,B) => Output[C])(
      implicit tr: ToResponse.Aux[Output[C], CT]) = {
      FilterRequestToService((filter: Filter[Request,Response,ReqExt[AA],Response]) => RequestToService(
        reqFn(m, Path(path)), Service.mk(filter.joinPath[B](path).andThen(abo =>
          abo match {
            case None => NotFoundException.Future
            case Some((reqExt, path_)) => mkResponse(tr(service(reqExt.value,path_)))
          }
        ))))
    }

    def apply[CC,D,CT <:String](rr: ReqRead[Future[CC]])(service: (AA,B,CC) => Future[Output[D]])(
      implicit tr: ToResponse.Aux[Output[D],CT]) = {
      FilterRequestToService((filter: Filter[Request,Response,ReqExt[AA],Response]) => RequestToService(
        reqFn(m, Path(path)), Service.mk(filter.andThen(rr.toFilter.joinPath2[B,AA](path)).andThen(abo =>
          abo match {
            case None => NotFoundException.Future
            case Some((a,c,b)) => service(a,b,c).flatMap(r=> mkResponse(tr(r)))
          }
        ))))
    }

    def apply[CC,D,CT <:String](rr: Lazy[ReqRead[CC]])(service: (AA,B,CC) => Future[Output[D]])(
      implicit tr: ToResponse.Aux[Output[D],CT]) = {
      FilterRequestToService((filter: Filter[Request,Response,ReqExt[AA],Response]) => RequestToService(
        reqFn(m, Path(path)), Service.mk(filter.andThen(rr.value.toFilter.joinPath2[B,AA](path)).andThen(abo =>
          abo match {
            case None => NotFoundException.Future
            case Some((a,c,b)) => service(a,b,c).flatMap(r=> mkResponse(tr(r)))
          }
        ))))
    }

    def apply[C, CT <: String](service: (AA,B) => Future[Output[C]])(
      implicit tr: ToResponse.Aux[Output[C], CT]) = {
      FilterRequestToService((filter: Filter[Request,Response,ReqExt[AA],Response]) => RequestToService(
        reqFn(m, Path(path)), Service.mk(filter.joinPath[B](path).andThen(abo =>
          abo match {
            case None => NotFoundException.Future
            case Some((reqExt, path_)) => service(reqExt.value,path_).flatMap(r=> mkResponse(tr(r)))
          }
        ))))
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

  def sync[C,CT <:String](service: B => Output[C])
                         (implicit tr: ToResponse.Aux[Output[C],CT] ) = {
    RequestToService(reqFn(m, Path(path)), Service.mk(path.toFilter.andThen(abo =>
      abo match {
        case None => NotFoundException.Future
        case Some((b,path_)) => mkResponse(tr(service(b)))
      }
    )))
  }

  def sync(filter: Filter[Request, Response, A, Response])(service: (B,A) => Future[Response]) =
    RequestToService(reqFn(m, Path(path)), Service.mk(filter.joinPath[B](path).andThen(abo =>
      abo match {
        case None => NotFoundException.Future
        case Some((a,b)) => service(b,a)
      }
    )))

  def sync[C,CT <:String](rr: Lazy[ReqRead[A]])(service: (B,A)=> Output[C])
                         (implicit tr: ToResponse.Aux[Output[C], CT]): RequestToService = {
    RequestToService(reqFn(m, Path(path)), Service.mk(rr.value.toFilter.joinPath[B](path).andThen(abo =>
      abo match {
        case None => NotFoundException.Future
        case Some((a,b)) => mkResponse(tr(service(b,a)))
      }
    )))
  }

  def sync[C,CT <:String](rr: ReqRead[Future[A]])(service: (B,A) => Output[C])
                         (implicit tr: ToResponse.Aux[Output[C], CT]): RequestToService = {
    RequestToService(reqFn(m, Path(path)), Service.mk(rr.toFilter.joinPath[B](path).andThen(abo =>
      abo match {
        case None => NotFoundException.Future
        case Some((a,b)) => mkResponse(tr(service(b,a)))
      }
    )))
  }


  def apply[C,CT <:String](service: B => Future[Output[C]])
                          (implicit tr: ToResponse.Aux[Output[C],CT] ) = {
    RequestToService(reqFn(m, Path(path)), Service.mk(path.toFilter.andThen(abo =>
      abo match {
        case None => NotFoundException.Future
        case Some((b,path_)) =>service(b).flatMap(r => mkResponse(tr(r)))
      }
    )))
  }

  def apply[C,CT <:String](rr: Lazy[ReqRead[A]])(service: (B,A)=> Future[Output[C]])
                          (implicit tr: ToResponse.Aux[Output[C], CT]): RequestToService = {
    RequestToService(reqFn(m, Path(path)), Service.mk(rr.value.toFilter.joinPath[B](path).andThen(abo =>
      abo match {
        case None => NotFoundException.Future
        case Some((a,b)) =>service(b,a).flatMap(r => mkResponse(tr(r)))
      }
    )))
  }

  def apply[C,CT <:String](rr: ReqRead[Future[A]])(service: (B,A) => Future[Output[C]])
                          (implicit tr: ToResponse.Aux[Output[C], CT]): RequestToService = {
    RequestToService(reqFn(m, Path(path)), Service.mk(rr.toFilter.joinPath[B](path).andThen(abo =>
      abo match {
        case None => NotFoundException.Future
        case Some((a,b)) => service(b,a).flatMap(r => mkResponse(tr(r)))
      }
    )))
  }

}

 */