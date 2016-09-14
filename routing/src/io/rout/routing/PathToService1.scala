package io.rout.routing

import com.twitter.finagle.{Filter, Service}
import com.twitter.finagle.http.{Method, Request, Response}
import com.twitter.io.Charsets
import com.twitter.util.Future
import io.rout.{Output, ReqReadExt, ReqRead, ToResponse}
import io.rout.path.Path
import shapeless.Lazy

//todo clean all this mess
case class PathToService[CT <:String](m: Method,path: Path){

  private def mkResponse(response: Response) = {
    response.charset = Charsets.Utf8.toString
    Future.value(response)
  }

  private def reqFn(method: Method,path: Path): Request => Boolean = (request: Request) =>
    if(method == request.method && Path.compare(path,request.path)) true else false

  def filter[B] = new filter[B] {}

  trait filter[B]{

    def sync[AA,C](rr: ReqRead[Future[AA]])(service: (B,AA) => Output[C])(
      implicit tr: ToResponse.Aux[Output[C],CT]) = {
      FilterRequestToService((filter: Filter[Request,Response,ReqReadExt[B],Response]) => RequestToService(
        reqFn(m, path), Service.mk(filter.andThen(rr.toFilter2[B]) andThen service.tupled.andThen(x=> mkResponse(tr(x))))))
    }

    def sync[AA,C](rr: Lazy[ReqRead[AA]])(service: (B,AA) => Output[C])(
      implicit tr: ToResponse.Aux[Output[C],CT]) = {
      FilterRequestToService((filter: Filter[Request,Response,ReqReadExt[B],Response]) => RequestToService(
        reqFn(m, path), Service.mk(filter.andThen(rr.value.toFilter2[B]) andThen service.tupled.andThen(x=> mkResponse(tr(x))))))
    }

    def sync[AA](service:  B => Output[AA])(
      implicit tr: ToResponse.Aux[Output[AA],CT]) =
      FilterRequestToService((filter: Filter[Request,Response,ReqReadExt[B],Response]) => RequestToService(
        reqFn(m,path),Service.mk(filter.andThen(aa=> service.andThen(x=> mkResponse(tr(x)))(aa.value)))))



    def apply[AA,C](rr: ReqRead[Future[AA]])(service: (B,AA) => Future[Output[C]])(
      implicit tr: ToResponse.Aux[Output[C],CT]) =
      FilterRequestToService((filter: Filter[Request,Response,ReqReadExt[B],Response]) => RequestToService(
        reqFn(m, path), Service.mk(filter.andThen(rr.toFilter2[B]) andThen service.tupled.andThen(fOut=> fOut.flatMap(outC=> mkResponse(tr(outC)))))))


    def apply[AA,C](rr: Lazy[ReqRead[AA]])(service: (B,AA) =>  Future[Output[C]])(
      implicit tr: ToResponse.Aux[Output[C],CT]) =
      FilterRequestToService((filter: Filter[Request,Response,ReqReadExt[B],Response]) => RequestToService(
        reqFn(m, path), Service.mk(filter.andThen(rr.value.toFilter2[B]) andThen service.tupled.andThen(fOut=> fOut.flatMap(outC => mkResponse(tr(outC)))))))

    def apply[AA](service:  B => Future[Output[AA]])(
      implicit tr: ToResponse.Aux[Output[AA],CT] ) =
      FilterRequestToService((filter: Filter[Request,Response,ReqReadExt[B],Response]) => RequestToService(
        reqFn(m,path),Service.mk(filter.andThen(aa=> service.andThen(fOut=> fOut.flatMap(out=> mkResponse(tr(out))))(aa.value)))))

    def partial[AA](service:  PartialFunction[B,Output[AA]])(
      implicit tr: ToResponse.Aux[Output[AA],CT] ) =
      FilterRequestToService((filter: Filter[Request,Response,ReqReadExt[B],Response]) => RequestToService(
        reqFn(m,path),Service.mk(filter.andThen(aa=> service.andThen(x=> mkResponse(tr(x)))(aa.value)))))
  }

  def service(service:  Request => Future[Response]) =
    RequestToService(reqFn(m,path),Service.mk(service))

  def service[A](rr: Lazy[ReqRead[A]])(service: A => Future[Response]) =
    RequestToService(reqFn(m,path),Service.mk(rr.value.toFilter andThen service))

  def service[A](rr: ReqRead[Future[A]])(service: A => Future[Response]) =
    RequestToService(reqFn(m,path),Service.mk(rr.toFilter andThen service))

  def sync[B](service:  Request => Output[B])(implicit tr: ToResponse.Aux[Output[B],CT] ) =
    RequestToService(reqFn(m,path),Service.mk(service.andThen(x=> mkResponse(tr(x)))))

  def sync[A](filter: Filter[Request,Response,A,Response])(service: A => Future[Response]) =
    RequestToService(reqFn(m,path),Service.mk(filter andThen service))

  def sync[B,A](rr: ReqRead[Future[A]])(service: A => Output[B])
                         (implicit tr: ToResponse.Aux[Output[B],CT] ) = {
    RequestToService(reqFn(m,path),Service.mk(rr.toFilter andThen service.andThen(x=> mkResponse(tr(x)))))
  }

  def sync[B,A](rr: Lazy[ReqRead[A]])(service: A => Output[B])
                         (implicit tr: ToResponse.Aux[Output[B],CT] ) = {
    RequestToService(reqFn(m,path),Service.mk(rr.value.toFilter andThen service.andThen(x=> mkResponse(tr(x)))))
  }

  def sync[B](service:  Output[B])(implicit tr: ToResponse.Aux[Output[B],CT] ) =
    RequestToService(reqFn(m,path),Service.mk[Request,Response](r=> mkResponse(tr(service))))

  def apply[B](service:  Request => Future[Output[B]])(implicit tr: ToResponse.Aux[Output[B],CT] ) =
    RequestToService(reqFn(m,path),Service.mk(service.andThen(fOut=> fOut.flatMap(outB =>  mkResponse(tr(outB))))))

  def apply[A](filter: Filter[Request,Response,A,Response])(service: A => Future[Response]) =
    RequestToService(reqFn(m,path),Service.mk(filter andThen service))

/*
  def apply[B,A](rr: ReqRead[Future[A]])(service: A => Output[B])
                          (implicit tr: ToResponse.Aux[Output[B],CT] ) =
    RequestToService(reqFn(m,path),Service.mk(rr.toFilter andThen service.andThen(outB=> mkResponse(tr(outB)))))
 */

  def apply[B,A](rr: ReqRead[Future[A]])(service: A => Future[Output[B]])
                (implicit tr: ToResponse.Aux[Output[B],CT] ) =
    RequestToService(reqFn(m,path),Service.mk(rr.toFilter andThen service.andThen(outB=> outB.flatMap(outB1 => mkResponse(tr(outB1))))))

  def apply[B,A](rr: Lazy[ReqRead[A]])(service: A => Future[Output[B]])
                          (implicit tr: ToResponse.Aux[Output[B],CT] ) = {
    RequestToService(reqFn(m,path),Service.mk(rr.value.toFilter andThen service.andThen(x=> x.map(xx=> tr(xx)).flatMap(r => mkResponse(r)))))
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


case class PathToService[A](m: Method,path: Path){

  private def mkResponse(response: Response) = {
    response.charset = Charsets.Utf8.toString
    Future.value(response)
  }

  private def reqFn(method: Method,path: Path): Request => Boolean = (request: Request) =>
    if(method == request.method && Path.compare(path,request.path)) true else false

  def filter[B] = new filter[B] {}

  trait filter[B]{

    def sync[AA,C,CT <:String](rr: ReqRead[Future[AA]])(service: (B,AA) => Output[C])(
      implicit tr: ToResponse.Aux[Output[C],CT]) = {
      FilterRequestToService((filter: Filter[Request,Response,ReqExt[B],Response]) => RequestToService(
        reqFn(m, path), Service.mk(filter.andThen(rr.toFilter2[B]) andThen service.tupled.andThen(x=> mkResponse(tr(x))))))
    }

    def sync[AA,C,CT <:String](rr: Lazy[ReqRead[AA]])(service: (B,AA) => Output[C])(
      implicit tr: ToResponse.Aux[Output[C],CT]) = {
      FilterRequestToService((filter: Filter[Request,Response,ReqExt[B],Response]) => RequestToService(
        reqFn(m, path), Service.mk(filter.andThen(rr.value.toFilter2[B]) andThen service.tupled.andThen(x=> mkResponse(tr(x))))))
    }

    def sync[AA,CT <:String](service:  B => Output[AA])(
      implicit tr: ToResponse.Aux[Output[AA],CT]) =
      FilterRequestToService((filter: Filter[Request,Response,ReqExt[B],Response]) => RequestToService(
        reqFn(m,path),Service.mk(filter.andThen(aa=> service.andThen(x=> mkResponse(tr(x)))(aa.value)))))



    def apply[AA,C,CT <:String](rr: ReqRead[Future[AA]])(service: (B,AA) => Future[Output[C]])(
      implicit tr: ToResponse.Aux[Output[C],CT]) = {
      FilterRequestToService((filter: Filter[Request,Response,ReqExt[B],Response]) => RequestToService(
        reqFn(m, path), Service.mk(filter.andThen(rr.toFilter2[B]) andThen service.tupled.andThen(x=> x.map(xx=> tr(xx)).flatMap(r => mkResponse(r))))))
    }

    def apply[AA,C,CT <:String](rr: Lazy[ReqRead[AA]])(service: (B,AA) =>  Future[Output[C]])(
      implicit tr: ToResponse.Aux[Output[C],CT]) = {
      FilterRequestToService((filter: Filter[Request,Response,ReqExt[B],Response]) => RequestToService(
        reqFn(m, path), Service.mk(filter.andThen(rr.value.toFilter2[B]) andThen service.tupled.andThen(x=> x.map(xx=> tr(xx)).flatMap(r => mkResponse(r))))))
    }


    def apply[AA,CT <:String](service:  B => Future[Output[AA]])(
      implicit tr: ToResponse.Aux[Output[AA],CT] ) =
      FilterRequestToService((filter: Filter[Request,Response,ReqExt[B],Response]) => RequestToService(
        reqFn(m,path),Service.mk(filter.andThen(aa=> service.andThen(x=> x.map(xx=> tr(xx)).flatMap(r => mkResponse(r)))(aa.value)))))

    def partial[AA,CT <:String](service:  PartialFunction[B,Output[AA]])(
      implicit tr: ToResponse.Aux[Output[AA],CT] ) =
      FilterRequestToService((filter: Filter[Request,Response,ReqExt[B],Response]) => RequestToService(
        reqFn(m,path),Service.mk(filter.andThen(aa=> service.andThen(x=> mkResponse(tr(x)))(aa.value)))))
  }

  def service(service:  Request => Future[Response]) =
    RequestToService(reqFn(m,path),Service.mk(service))

  def service(rr: Lazy[ReqRead[A]])(service: A => Future[Response]) =
    RequestToService(reqFn(m,path),Service.mk(rr.value.toFilter andThen service))

  def service(rr: ReqRead[Future[A]])(service: A => Future[Response]) =
    RequestToService(reqFn(m,path),Service.mk(rr.toFilter andThen service))

  def sync[B,CT <:String](service:  Request => Output[B])(implicit tr: ToResponse.Aux[Output[B],CT] ) =
    RequestToService(reqFn(m,path),Service.mk(service.andThen(x=> mkResponse(tr(x)))))

  def sync(filter: Filter[Request,Response,A,Response])(service: A => Future[Response]) =
    RequestToService(reqFn(m,path),Service.mk(filter andThen service))

  def sync[B,CT <:String](rr: ReqRead[Future[A]])(service: A => Output[B])
                         (implicit tr: ToResponse.Aux[Output[B],CT] ) = {
    RequestToService(reqFn(m,path),Service.mk(rr.toFilter andThen service.andThen(x=> mkResponse(tr(x)))))
  }

  def sync[B,CT <:String](rr: Lazy[ReqRead[A]])(service: A => Output[B])
                         (implicit tr: ToResponse.Aux[Output[B],CT] ) = {
    RequestToService(reqFn(m,path),Service.mk(rr.value.toFilter andThen service.andThen(x=> mkResponse(tr(x)))))
  }


  def apply[B,CT <:String](service:  Request => Future[Output[B]])(implicit tr: ToResponse.Aux[Output[B],CT] ) =
    RequestToService(reqFn(m,path),Service.mk(service.andThen(x=> x.map(xx=> tr(xx)).flatMap(r => mkResponse(r)))))

  def apply(filter: Filter[Request,Response,A,Response])(service: A => Future[Response]) =
    RequestToService(reqFn(m,path),Service.mk(filter andThen service))

  def apply[B,CT <:String](rr: ReqRead[Future[A]])(service: A => Future[Output[B]])
                          (implicit tr: ToResponse.Aux[Output[B],CT] ) = {
    RequestToService(reqFn(m,path),Service.mk(rr.toFilter andThen service.andThen(x=> x.map(xx=> tr(xx)).flatMap(r => mkResponse(r)))))
  }

  def apply[B,CT <:String](rr: Lazy[ReqRead[A]])(service: A => Future[Output[B]])
                          (implicit tr: ToResponse.Aux[Output[B],CT] ) = {
    RequestToService(reqFn(m,path),Service.mk(rr.value.toFilter andThen service.andThen(x=> x.map(xx=> tr(xx)).flatMap(r => mkResponse(r)))))
  }

}

 */