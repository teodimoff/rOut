package io.rout

import com.twitter.concurrent.AsyncStream
import com.twitter.finagle.http.{Cookie, Request}
import com.twitter.finagle.http.exp.Multipart.FileUpload
import com.twitter.finagle.netty3.ChannelBufferBuf
import com.twitter.io.{Buf, Charsets}
import com.twitter.util.{Future, Try}

trait ReqReads {

  import items._

  /**
   * Implicit conversion that allows the same inline rules to be used for required and optional values. If the optional
   * value is non-empty, it gets validated (and validation may fail, producing error), but if it is empty, it is always
   * treated as valid.
   *
   * In order to help the compiler determine the case when inline rule should be converted, the type of the validated
   * value should be specified explicitly.
   *
   * {{{
   *   paramOption("foo").should("be greater than 50") { i: Int => i > 50 }
   * }}}
   *
   * @param fn the underlying function to convert
   */
  implicit def toOptionalInlineRule[A](fn: A => Boolean): Option[A] => Boolean = {
    case Some(value) => fn(value)
    case None => true
  }

  // Helper functions.
  private[rout] def requestParam(param: String)(req: Request): Option[String] =
    req.params.get(param).orElse(req.multipart.flatMap(m => m.attributes.get(param).flatMap(_.headOption)))

  private[rout] def requestParams(params: String)(req: Request): Seq[String] =
    req.params.getAll(params).toList.flatMap(_.split(","))

  private[rout] def requestHeader(header: String)(req: Request): Option[String] =
    req.headerMap.get(header)

  private[rout] def requestCookie(cookie: String)(req: Request): Option[Cookie] =
    req.cookies.get(cookie)

  private[rout] def requestUpload(upload: String)(req: Request): Option[FileUpload] =
    Try(req.multipart).getOrElse(None).flatMap(m => m.files.get(upload).flatMap(fs => fs.headOption))

  private[rout]def matches[A](item: RequestItem)(p:Request => Boolean)(f: Request => A): ReqRead[A] =
  ReqRead.embed(item)(input =>
   if(p(input)) Future.value(Some(f(input))) else Future.None).failIfNone
  // A convenient method for internal needs.
  private[rout] def rr[A](i: RequestItem)(f: Request => A): ReqRead[A] =
    ReqRead.embed[A](i)(r => Future.value(f(r)))

  /**
   * Creates a [[ReqRead]] that reads a required query-string param `name` from the request or raises an
   * [[Error.NotPresent]] exception when the param is missing; an [[Error.NotValid]] exception is the param is empty.
   *
   * @param name the param name to read
   *
   * @return a param value
   */

  def param(name: String): ReqRead[String] =
    rr(ParamItem(name))(requestParam(name)).failIfNone.shouldNot(beEmpty)

  /**
   * Creates a [[ReqRead]] that reads an optional query-string param `name` from the request into an `Option`.
   *
   * @param name the param to read
   *
   * @return an `Option` that contains a param value or `None` if the param is empty
   */
  def paramOption(name: String): ReqRead[Option[String]] =
    rr(ParamItem(name))(requestParam(name)).noneIfEmpty

  /**
   * Creates a [[ReqRead]] that reads a required (in a meaning that a resulting `Seq` will have at least one
   * element) multi-value query-string param `name` from the request into a `Seq` or raises a [[Error.NotPresent]]
   * exception when the params are missing or empty.
   *
   * @param name the param to read
   *
   * @return a `Seq` that contains all the values of multi-value param
   */
  def paramsNonEmpty(name: String): ReqRead[Seq[String]] =
    rr(ParamItem(name))(requestParams(name)).embedFlatMap({
      case Nil => Future.exception(Error.NotPresent(ParamItem(name)))
      case unfiltered => Future.value(unfiltered.filter(_.nonEmpty))
    }).shouldNot("be empty")(_.isEmpty)

  /**
   * Creates a [[ReqRead]] that reads an optional (in a meaning that a resulting `Seq` may be empty) multi-value
   * query-string param `name` from the request into a `Seq`.
   *
   * @param name the param to read
   *
   * @return a `Seq` that contains all the values of multi-value param or an empty seq `Nil` if the params are missing
   *         or empty.
   */
  def params(name: String): ReqRead[Seq[String]] =
    rr(ParamItem(name))(requestParams(name)(_).filter(_.nonEmpty))

  def paramFn[A](name: String)(f: String => Future[Option[A]]
  ):ReqRead[Future[Option[A]]] = param(name).map(f)

  def paramsFn[A](name: String)(f: Seq[String] => Future[Option[Seq[A]]]
  ):ReqRead[Future[Option[Seq[A]]]] = params(name).map(f)

  /**
   * Creates a [[ReqRead]] that reads a required HTTP header `name` from the request or raises an
   * [[Error.NotPresent]] exception when the header is missing.
   *
   * @param name the header to read
   *
   * @return a header
   */
  def header(name: String): ReqRead[String] =
    rr(HeaderItem(name))(requestHeader(name)).failIfNone.shouldNot(beEmpty)

  /**
   * Creates a [[ReqRead]] that reads an optional HTTP header `name` from the request into an `Option`.
   *
   * @param name the header to read
   *
   * @return an `Option` that contains a header value or `None` if the header is not present in the request
   */
  def headerOption(name: String): ReqRead[Option[String]] =
    rr(HeaderItem(name))(requestHeader(name)).noneIfEmpty

  /**
   * A [[ReqRead]] that reads a binary request body, interpreted as a `Array[Byte]`, into an `Option`.
   */
  val binaryBodyOptionArray: ReqRead[Option[Array[Byte]]] =
    matches(BodyItem)(!_.isChunked)(req =>
    req.contentLength match {
      case Some(n) if n > 0 => Some(Buf.ByteArray.Shared.extract(req.content))
      case _ => None
    }
  )

  /**
    * A [[ReqRead]] that reads a binary request body, interpreted as a `Buf`, into an `Option`.
    */
  val binaryBodyOption: ReqRead[Option[Buf]] =
    matches(BodyItem)(!_.isChunked)(req =>
      req.contentLength match {
        case Some(n) if n > 0 => Some(req.content)
        case _ => None
      }
    )

  /**
    * A [[ReqRead]] that reads a required binary request body, interpreted as a `Buf`, or throws a
    * [[Error.NotPresent]] exception.
    */
  val binaryBody: ReqRead[Buf] = binaryBodyOption.failIfNone


  /**
   * A [[ReqRead]] that reads a required binary request body, interpreted as a `Array[Byte]`, or throws a
   * [[Error.NotPresent]] exception.
   */
  val binaryBodyArray: ReqRead[Array[Byte]] = binaryBodyOptionArray.failIfNone

  /**
   * A [[ReqRead]] that reads an optional request body, interpreted as a `String`, into an `Option`.
   */
  val bodyOption: ReqRead[Option[String]] = rr(BodyItem)(req =>
    req.contentLength match {
      case Some(n) if n > 0 =>
        val buffer = ChannelBufferBuf.Owned.extract(req.content)
        // Note: We usually have an array underneath the ChannelBuffer (at least on Netty 3).
        // This check is mostly about a safeguard.
        if (buffer.hasArray) Some(new String(buffer.array(), 0, buffer.readableBytes(), "UTF-8"))
        else Some(buffer.toString(Charsets.Utf8))
      case _ => None
    }
  )

  /**
   * A [[ReqRead]] that reads the required request body, interpreted as a `String`, or throws an
   * [[Error.NotPresent]] exception.
   */
  val body: ReqRead[String] = bodyOption.failIfNone

  val asyncBody: ReqRead[AsyncStream[Buf]] =
  matches(items.BodyItem)(_.isChunked)(req=> AsyncStream.fromReader(req.reader))

  /**
   * Creates a [[ReqRead]] that reads an optional HTTP cookie from the request into an `Option`.
   *
   * @param name the name of the cookie to read
   *
   * @return an `Option` that contains a cookie or None if the cookie does not exist on the request.
   */
  def cookieOption(name: String): ReqRead[Option[Cookie]] = rr(CookieItem(name))(requestCookie(name))

  /**
   * Creates a [[ReqRead]] that reads a required cookie from the request or raises an [[Error.NotPresent]]
   * exception when the cookie is missing.
   *
   * @param name the name of the cookie to read
   *
   * @return the cookie
   */
  def cookie(name: String): ReqRead[Cookie] = cookieOption(name).failIfNone

  /**
   * Creates a [[ReqRead]] that reads an optional file upload from a multipart/form-data request into an `Option`.
   *
   * @param name the name of the parameter to read
   * @return an `Option` that contains the file or `None` is the parameter does not exist on the request.
   */
  def fileUploadOption(name: String): ReqRead[Option[FileUpload]] =
    matches(ParamItem(name))(!_.isChunked)(requestUpload(name))

  /**
   * Creates a [[ReqRead]] that reads a required file upload from a multipart/form-data request.
   *
   * @param name the name of the parameter to read
   * @return the file
   */
  def fileUpload(name: String): ReqRead[FileUpload] = fileUploadOption(name).failIfNone

  private[rout] val beEmpty: ValidationRule[String] = ValidationRule("be empty")(_.isEmpty)
}
