package io.rout

import cats.data.Xor

import scala.reflect.ClassTag
import com.twitter.finagle.http.Request
import com.twitter.io.Buf
import com.twitter.util.{Future, Return, Throw, Try}
import shapeless.{::, Generic, HList, HNil, Lazy}
import shapeless.ops.function.FnToProduct
import shapeless.ops.hlist.Tupler

import scala.language.higherKinds

/**
 * A `ReqRead` (a reader monad) that reads a [[Future]] of `A` from the request of type `R`. `ReqRead`s
 * might be composed with each other using either monadic API (`flatMap` method) or applicative API (`::` method).
 * Regardless the API used for `ReqRead`s composition, the main idea behind it is to use primitive readers (i.e.,
 * `param`, `paramOption`) in order to _compose_ them together and _map_ to the application domain data.
 *
 * {{{
 *   case class Complex(r: Double, i: Double)
 *   val complex: ReqRead[Complex] = (
 *     param("real").asText[Double] ::
 *     paramOption("imaginary").as[Double].default(0.0)
 *   ).as[Complex]
 * }}}
 *
 */

//this was RequestReader once upon a time
trait ReqRead[A] { self =>

  import items._

  /**
   * A [[RequestItem]] read by this request reader.
   */
  def item: RequestItem

  /**
   * Reads the data from given request `req`.
   *
   * @param req the request to read
   */
  def apply(req: Request): Future[A]

  /**
   * Flat-maps this request reader to the given function `A => ReqRead[B]`.
   */
  def flatMap[B](fn: A => ReqRead[B]): ReqRead[B] = new ReqRead[B] {
    val item = MultipleItems
    def apply(req: Request): Future[B] = self(req).flatMap(a => fn(a)(req))
  }

  /**
   * Maps this request reader to the given function `A => B`.
   */
  def map[B](fn: A => B): ReqRead[B] = new ReqRead[B] {
    val item = self.item
    def apply(req: Request): Future[B] = self(req).map(fn)
  }

/*
  /**
    * Joins this request reader with rrb.
    */
  def join[B](rrb:Lazy[ReqRead[B]]):ReqRead[(A,B)] =
    self.flatMap(ra=> rrb.value.map(ba => ra -> ba))


  def join[B](rrb:ReqRead[Future[B]]):ReqRead[Future[(A,B)]] =
    self.flatMap(ra=> rrb.map(ba => ba.map(baa => ra -> baa)))

 */

  /**
   * Flat-maps this request reader to the given function `A => Future[B]`.
   */
  def embedFlatMap[B](fn: A => Future[B]): ReqRead[B] = new ReqRead[B] {
    val item = self.item
    def apply(req: Request): Future[B] = self(req).flatMap(fn)
  }

  /**
   * Applies the given filter `p` to this request reader.
   */
  def filter(p: A => Boolean): ReqRead[A] = self.should("not fail validation")(p)

  //use filter implicit conversion
  private def filterf(p: A => Future[Boolean]): ReqRead[A] = self.shouldF("not fail validation")(p)

  private def shouldf(rule: String)(p: A => Future[Boolean]): ReqRead[A] = self.shouldF(rule)(p)

  /**
   * Lifts this request reader into one that always succeeds, with an empty option representing failure.
   */
  def lift: ReqRead[Option[A]] = new ReqRead[Option[A]] {
    val item = self.item
    def apply(req: Request): Future[Option[A]] = self(req).liftToTry.map(_.toOption)
  }

  /**
   * Validates the result of this request reader using a `predicate`. The rule is used for error reporting.
   *
   * @param rule text describing the rule being validated
   * @param predicate returns true if the data is valid
    * @return a request reader that will return the value of this reader if it is valid.
   *         Otherwise the future fails with an [[Error.NotValid]] error.
   */
  def should(rule: String)(predicate: A => Boolean): ReqRead[A] = embedFlatMap(a =>
    if (predicate(a)) Future.value(a)
    else Future.exception(Error.NotValid(self.item, "should " + rule))
  )

  def shouldF(rule: String)(predicate: A => Future[Boolean]): ReqRead[A] = embedFlatMap(a =>
    predicate(a) flatMap (x => if (x) Future.value(a)
    else Future.exception(Error.NotValid(self.item, "should " + rule)))
  )

  /**
   * Validates the result of this request reader using a `predicate`. The rule is used for error reporting.
   *
   * @param rule text describing the rule being validated
   * @param predicate returns false if the data is valid
    * @return a request reader that will return the value of this reader if it is valid.
   *         Otherwise the future fails with a [[Error.NotValid]] error.
   */
  def shouldNot(rule: String)(predicate: A => Boolean): ReqRead[A] = should(s"not $rule.")(x => !predicate(x))

  def shouldNotF(rule: String)(predicate: A => Future[Boolean]): ReqRead[A] =
    shouldF(s"not $rule.")(x => predicate(x).map(b=> !b))


  /**
   * Validates the result of this request reader using a predefined `rule`. This method allows for rules to be reused
   * across multiple request readers.
   *
   * @param rule the predefined [[ValidationRule]] that will return true if the data is
   *             valid
    * @return a request reader that will return the value of this reader if it is valid.
   *         Otherwise the future fails with an [[Error.NotValid]] error.
   */
  def should[B](rule: ValidationRule[A]): ReqRead[A] = should(rule.description)(rule.apply)

  /**
   * Validates the result of this request reader using a predefined `rule`. This method allows for rules to be reused
   * across multiple request readers.
   *
   * @param rule the predefined [[ValidationRule]] that will return false if the data is
   *             valid
    * @return a request reader that will return the value of this reader if it is valid.
   *         Otherwise the future fails with a [[Error.NotValid]] error.
   */
  def shouldNot(rule: ValidationRule[A]): ReqRead[A] = shouldNot(rule.description)(rule.apply)
}

/**
 * Convenience methods for creating new reader instances.
 */
object ReqRead {

  import items._

  /**
   * Creates a new [[ReqRead]] that always succeeds, producing the specified value.
   *
   * @param value the value the new reader should produce
   * @return a new reader that always succeeds, producing the specified value
   */
  def value[A](value: A): ReqRead[A] = const[A](Future.value(value))

  /**
   * Creates a new [[ReqRead]] that always fails, producing the specified
   * exception.
   *
   * @param exc the exception the new reader should produce
   * @return a new reader that always fails, producing the specified exception
   */
  def exception[A](exc: Throwable): ReqRead[A] = const[A](Future.exception(exc))

  /**
   * Creates a new [[ReqRead]] that always produces the specified value. It will
   * succeed if the given `Future` succeeds and fail if the `Future` fails.
   *
   * @param value the value the new reader should produce
   * @return a new reader that always produces the specified value
   */
  def const[A](value: Future[A]): ReqRead[A] = embed[A](MultipleItems)(_ => value)

  /**
   * Creates a new [[ReqRead]] that reads the result from the request.
   *
   * @param f the function to apply to the request
   * @return a new reader that reads the result from the request
   */
  def apply[A](f: Request => A): ReqRead[A] = embed[A](MultipleItems)(req => Future.value(f(req)))

  private[rout] def embed[A](i: RequestItem)(f: Request => Future[A]): ReqRead[A] =
    new ReqRead[A] {
      val item = i
      def apply(req: Request): Future[A] = f(req)
    }

  private[this] def notParsed[A](rr: ReqRead[_], tag: ClassTag[_]): PartialFunction[Throwable, Try[A]] = {
    case exc => Throw[A](Error.NotParsed(rr.item, tag, exc))
  }

  /**
   * Implicit conversion that allows to call `as[A]` on any `ReqRead[String]` to perform a type conversion based
   * on an implicit `Decode[A]` which must be in scope.
   *
   * The resulting reader will fail when type conversion fails.
   */
  implicit class StringReaderOps(val rr: ReqRead[String]) extends AnyVal {

    def asText[A](implicit decoder: Decode.TextPlain[String,A], tag: ClassTag[A]): ReqRead[A] = decode

    def asJson[A](implicit decoder: Decode.ApplicationJson[String,A], tag: ClassTag[A]): ReqRead[A] = decode

    def asXml[A](implicit decoder: Decode.ApplicationXml[String,A], tag: ClassTag[A]): ReqRead[A] = decode

    def decode[A,CT <: String](implicit decoder: Decode.Aux[String,A,CT], tag: ClassTag[A]): ReqRead[A] =
      rr.embedFlatMap(value => decoder(value) match {
        case Xor.Right(Return(r)) => Future.value(r)
        case Xor.Right(Throw(t)) => Future.exception(Error.NotParsed(rr.item,tag,t))
        case Xor.Left(e) => Future.exception(e)
      })
  }

  implicit class BufReaderOps(val rr: ReqRead[Buf]) extends AnyVal {

    def asJson[A](implicit decoder: Decode.ApplicationJson[Buf,A], tag: ClassTag[A]): ReqRead[A] = decode(decoder,tag)

    def decode[A,CT <: String](implicit decoder: Decode.Aux[Buf,A,CT], tag: ClassTag[A]): ReqRead[A] =
      rr.embedFlatMap(value => decoder(value) match {
        case Xor.Right(Return(r)) => Future.value(r)
        case Xor.Right(Throw(t)) => Future.exception(Error.NotParsed(rr.item,tag,t))
        case Xor.Left(e) => Future.exception(e)
      })
  }

  /**
   * Implicit conversion that allows to call `as[A]` on any `ReqRead[Option[String]]` to perform a type conversion
   * based on an implicit `Decode[A]` which must be in scope.
   *
   * The resulting reader will fail when the result is non-empty and type conversion fails. It will succeed if the
   * result is empty or type conversion succeeds.
   */
  implicit class StringOptionReaderOps(val rr: ReqRead[Option[String]]) extends AnyVal {

    def asText[A](implicit
      decoder: Decode.TextPlain[String,A],
      tag: ClassTag[A]): ReqRead[Option[A]] = decode

    def asJson[A](implicit
      decoder: Decode.ApplicationJson[String,A],
      tag: ClassTag[A]): ReqRead[Option[A]] = decode

    def decode[A,CT <: String](implicit
      decoder: Decode.Aux[String,A,CT],
      tag: ClassTag[A]): ReqRead[Option[A]] = rr.embedFlatMap {
        case Some(value) => decoder(value) match {
          case Xor.Right(r) => Future.value(r.toOption)
          case Xor.Left(e) => Future.None
      }
        case None => Future.None
    }

    private[rout] def noneIfEmpty: ReqRead[Option[String]] = rr.map {
      case Some(value) if value.isEmpty => None
      case other => other
    }
  }

  /**
   * Implicit conversion that allows to call `as[A]` on any `ReqRead[Seq[String]]` to perform a type conversion
   * based on an implicit ''Decode[A]'' which must be in scope.
   *
   * The resulting reader will fail when the result is non-empty and type conversion fails on one or more of the
   * elements in the `Seq`. It will succeed if the result is empty or type conversion succeeds for all elements.
   */
  implicit class StringSeqReaderOps(val rr: ReqRead[Seq[String]]) extends AnyVal{

    def asText[A](implicit
      decoder: Decode.TextPlain[String,A],
      tag: ClassTag[A]): ReqRead[Seq[A]] = decode

    def asJson[A](implicit
      decoder: Decode.ApplicationJson[String,A],
      tag: ClassTag[A]): ReqRead[Seq[A]] = decode

    def decode[A,CT <: String](implicit decoder: Decode.Aux[String,A,CT], tag: ClassTag[A]): ReqRead[Seq[A]] =
      rr.embedFlatMap { items =>
        val converted = items.flatMap(x=> decoder(x).toOption)
        if (converted.forall(_.isReturn)) Future.value(converted.map(_.get))
        else Future.exception(Error.RequestErrors(converted.collect {
          case Throw(e) => Error.NotParsed(rr.item, tag, e)
        }))
      }

    def toOption: ReqRead[Option[Seq[String]]] = rr.map(x=> Option(x))
  }

  /**
   * Implicit conversion that adds convenience methods to readers for optional values.
   */
  implicit class OptionReaderOps[A](val rr: ReqRead[Option[A]]) extends AnyVal {
    private[rout] def failIfNone: ReqRead[A] = rr.embedFlatMap {
      case Some(value) => Future.value(value)
      case None => Future.exception(Error.NotPresent(rr.item))
    }

    /**
     * If reader is empty it will return provided default value
     */
    def default[B >: A](default: => B): ReqRead[B] = rr.map(_.getOrElse(default))

    /**
     * If reader is empty it will return provided alternative
     */
    def orElse[B >: A](alternative: => Option[B]): ReqRead[Option[B]] = rr.map(_.orElse(alternative))
  }

  implicit class ReqReadFutureOps[A](val rr: ReqRead[Future[A]]) extends AnyVal {
    def filterf(f: A => Boolean) = rr.filterf(a => a.map(f))

    def must(rule: String)(f: A => Boolean) = rr.shouldf(rule)(a => a.map(f))

    def mustNot(rule: String)(f: A => Boolean) = rr.shouldNotF(rule)(a => a.map(f))

  }

  /**
   * Implicit class that provides `::` and other operations on any request reader that returns a
   * [[shapeless.HList]].
   *
   * See the implementation note on [[StringSeqReaderOps]] for a discussion of why this is not
   * currently a value class.
   */
  final implicit class HListReaderOps[L <: HList](val self: ReqRead[L]) {
    /**
     * Composes this request reader with the given `that` request reader.
     */
    def ::[S, A](that: ReqRead[A]): ReqRead[A :: L] =
      new ReqRead[A :: L] {
        val item = MultipleItems
        def apply(req: Request): Future[A :: L] =
          Future.join(that(req).liftToTry, self(req).liftToTry).flatMap {
            case (Return(a), Return(l)) => Future.value(a :: l)
            case (Throw(a), Throw(l)) => Future.exception(collectExceptions(a, l))
            case (Throw(e), _) => Future.exception(e)
            case (_, Throw(e)) => Future.exception(e)
          }

        def collectExceptions(a: Throwable, b: Throwable): Error.RequestErrors = {
          def collect(e: Throwable): Seq[Throwable] = e match {
            case Error.RequestErrors(errors) => errors
            case other => Seq(other)
          }

          Error.RequestErrors(collect(a) ++ collect(b))
        }
      }


    /**
     * Converts this request reader to one that returns any type with this [[shapeless.HList]] as
     * its representation.
     */
    def as[A](implicit gen: Generic.Aux[A, L]): ReqRead[A] = self.map(gen.from)

    /**
     * Converts this request reader to one that returns a tuple with the same types as this
     * [[shapeless.HList]].
     *
     * Note that this will fail at compile time if this this [[shapeless.HList]] contains more than
     * 22 elements.
     */
    def asTuple(implicit tupler: Tupler[L]): ReqRead[tupler.Out] = self.map(tupler(_))

    /**
     * Applies a `FunctionN` with the appropriate arity and types and a `Future` return type to
     * the elements of this [[shapeless.HList]].
     */
    def ~~>[F, I, FI](fn: F)(
      implicit ftp: FnToProduct.Aux[F, L => FI], ev: FI <:< Future[I]
      ): ReqRead[I] = self.embedFlatMap(value => ev(ftp(fn)(value)))

    /**
     * Applies a `FunctionN` with the appropriate arity and types to the elements of this
     * [[shapeless.HList]].
     */
    def ~>[F, I](fn: F)(implicit ftp: FnToProduct.Aux[F, L => I]): ReqRead[I] =
      self.map(ftp(fn))
  }

  /**
   * Implicit class that provides `::` and other operations on any request reader to support
   * building [[shapeless.HList]] request readers.
   */
  final implicit class ValueReaderOps[B](val self: ReqRead[B]) extends AnyVal {

    /**
     * Lift this request reader into a singleton [[shapeless.HList]] and compose it with the given
     * `that` request reader.
     */
    def ::[A](that: ReqRead[A]): ReqRead[A :: B :: HNil] =
      that :: self.map(_ :: HNil)

    /**
     * Converts this request reader to one that returns any type with `B :: HNil` as
     * its representation.
     */
    def as[A](implicit gen: Generic.Aux[A, B :: HNil]): ReqRead[A] = self.map { value =>
      gen.from(value :: HNil)
    }

    /**
     * Applies a function returning a future to the result of this reader.
     */
    def ~~>[A](fn: B => Future[A]): ReqRead[A] = self.embedFlatMap(fn)

    /**
     * Applies a function to the result of this reader.
     */
    def ~>[A](fn: B => A): ReqRead[A] = self.map(fn)
  }

}
