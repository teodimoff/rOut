package io.rout

import com.twitter.finagle.{Filter, Service}
import com.twitter.finagle.http.{Request, Response}
import com.twitter.util.Future
import io.rout.path.Path
import shapeless.Lazy
import scala.reflect.ClassTag

package object routing extends ReqReads {

 // val async = io.rout.asyncBody.toFilter

  implicit class FilterOps[A](val filter: Filter[Request, Response,A, Response]) extends AnyVal {
    def joinPath[B](path: Path => Option[(B,Path)]) = new Filter[Request, Response,Option[(A,B)], Response] {
      def apply(request: Request, service: Service[Option[(A,B)], Response]) =
        filter.andThen(a=> service(path(Path(request.path)).map(b => (a,b._1))))(request)
    }

    def joinPath2[B,C](path: Path => Option[(B,Path)]) = new Filter[ReqExt[C], Response,Option[(C,A,B)], Response] {
      def apply(requestExt: ReqExt[C], service: Service[Option[(C,A,B)], Response]) =
        filter.andThen(a=>
          service(path(Path(requestExt.request.path)).map(b => (requestExt.value,a,b._1))))(requestExt.request)
    }
  }

  implicit class PathOps[A](val path: Path => Option[A]) extends AnyVal {
    def toFilter = new Filter[Request, Response,Option[A], Response] {
      def apply(request: Request, service: Service[Option[A], Response]) =
        service(path(Path(request.path)))
    }
  }

  implicit class ReqReadOps[A](val rr: ReqRead[A]) extends AnyVal {
    def toFilter = new Filter[Request, Response, A, Response] {
      def apply(request: Request, service: Service[A, Response]) =
        rr(request) flatMap (a => service(a))
    }

    def toFilter2[B] = new Filter[ReqExt[B], Response, (B,A), Response] {
      def apply(requestExt: ReqExt[B], service: Service[(B,A), Response]) =
        rr(requestExt.request) flatMap (a => service((requestExt.value,a)))
    }

    def joinOption[B](b:ReqRead[B]):ReqRead[(A,Option[B])] =
      rr.flatMap(ra=> b.lift.map(ba => ra -> ba))

    def join[B](b:ReqRead[B]):ReqRead[(A,B)] =
      rr.flatMap(ra=> b.map(ba => ra -> ba))
  }

  implicit class ReqReadOpsOption[A](val rr: ReqRead[Option[A]]) extends AnyVal {
    def toFilter = new Filter[Request, Response, A, Response] {
      def apply(request: Request, service: Service[A, Response]) =
        rr(request) flatMap {
          case Some(x) => service(x)
        }
    }
  }

  implicit class ReqReadFuture[A](val rr: ReqRead[Future[A]]) extends AnyVal {
    def toFilter = new Filter[Request, Response, A, Response] {
      def apply(request: Request, service: Service[A, Response]) =
        rr(request) flatMap (a => a flatMap (aa => service(aa)))
    }

    def joinOption[B](bfuture:ReqRead[Future[B]]): ReqRead[Future[(A,Option[B])]] =
      rr.flatMap(ra => bfuture.lift.map(ba => ra join ba.swap))

    def join[B](b:ReqRead[Future[B]]):ReqRead[Future[(A,B)]] =
      rr.flatMap(ra=> b.map(ba => ra.join(ba) map(a => a)))
  }

  implicit class ReqReadOpsOptionFuture[A](val rr: ReqRead[Option[Future[A]]]) extends AnyVal {
    def toFilter = new Filter[Request, Response,A, Response] {
      def apply(request: Request, service: Service[A, Response]) =
        rr(request) flatMap  {
          case Some(fa) => fa flatMap(a=> service(a))
        }
    }

    def joinOption[B](b:ReqRead[B]):ReqRead[Option[Future[(A,Option[B])]]] =
      rr.flatMap(ofa => b.lift.map(ba => ofa.map(fa => fa.map((_, ba)))))


    def join[B](b:ReqRead[B]):ReqRead[Option[Future[(A,B)]]] =
      rr.flatMap(ra=> b.map(ba => ra map(a => a.map(x => x -> ba))))

  }

  implicit class ReqReadFn[B,A](val rr: ReqRead[B => A]) extends AnyVal {
    def embed(b: Lazy[ReqRead[B]]): ReqRead[A] =
      rr.flatMap(ra => b.value.map(ba => ra(ba)))

    def embed(bfuture: ReqRead[Future[B]]): ReqRead[Future[A]] =
      rr.flatMap(ra => bfuture.map(ba => ba.map(x=> ra(x))))

    def embed(paramName: String)(implicit dr: Decode.TextPlain[String,B],ct: ClassTag[B]): ReqRead[A] =
      rr.flatMap(ra => param(paramName).asText[B].map(ba => ra(ba)))

    def embed(b: B) = rr.map(_(b))

    def embed(b: Future[B]) = rr.map(x => b.map(x.apply))
  }

  implicit class ReqReadFnFuture[B,A](val rr: ReqRead[Future[B => A]]) extends AnyVal {
    def embed(b: Lazy[ReqRead[B]]): ReqRead[Future[A]] =
      rr.flatMap(ra => b.value.map(ba => ra map(ab => ab(ba))))

    def embed(bfuture:ReqRead[Future[B]]): ReqRead[Future[A]] =
      rr.flatMap(ra => bfuture.map(ba => ra.join(ba).map(x=> x._1(x._2))))

    def embed(b: B) = rr.map(_.map(_(b)))

    def embed(b: Future[B]) = rr.map(f => b join f map(x=> x._2(x._1)))
  }

  implicit class futureSwap[A](val future: Option[Future[A]]) extends AnyVal {
    def swap: Future[Option[A]] = future match {
      case None => Future.None
      case Some(fa) => fa.map(Option.apply)
    }
  }

  implicit class futureSwapFlat1[A](val future: Option[Future[Option[A]]]) extends AnyVal {
    def swapFlat: Future[Option[A]] = future.swap.swapFlat
  }

  implicit class futureSwapFlat2[A](val future: Future[Option[Future[A]]]) extends AnyVal {
    def swapFlat: Future[Option[A]] = future flatMap (_.swap)
  }

  implicit class futureSwapFlat3[A](val future: Future[Option[Option[A]]]) extends AnyVal {
    def swapFlat: Future[Option[A]] = future map (_.flatten)
  }


}
