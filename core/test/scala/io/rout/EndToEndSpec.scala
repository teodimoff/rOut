package io.rout

import cats.data.Xor
import com.twitter.finagle.Service
import com.twitter.finagle.http.{Request, Response, Status}
import com.twitter.io.Buf
import com.twitter.util.{Await, Return, Try}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EndToEndSpec extends RoutSpec {

  "A Coproduct Endpoint" should "be convertible into a Service" in {
    case class Foo(s: String)

    implicit val encodeFoo: Encode.TextPlain[Foo] =
      Encode.textPlain(s => Buf.Utf8(s.s))

    implicit val decodeFoo: Decode.TextPlain[String,Foo] =
      Decode.textPlain(s => Xor.Right(Try(Foo(s))))

    implicit val encodeException: Encode.TextPlain[Exception] =
      Encode.textPlain(_ => Buf.Utf8("ERR!"))

/*
    val service: Service[Request, Response] = (
      get("foo" / string) { s: String => Ok(Foo(s)) } :+:
      get("bar") { Created("bar") } :+:
      get("baz") { BadRequest(new IllegalArgumentException("foo")): Output[Unit] } :+:
      get("qux" ? param("foo").as[Foo]) { f: Foo => Created(f) }
    ).toService

        val rep1 = Await.result(service(Request("/foo/bar")))
    rep1.contentString shouldBe "bar"
    rep1.status shouldBe Status.Ok

    val rep2 = Await.result(service(Request("/bar")))
    rep2.contentString shouldBe "bar"
    rep2.status shouldBe Status.Created

    val rep3 = Await.result(service(Request("/baz")))
    rep3.contentString shouldBe "ERR!"
    rep3.status shouldBe Status.BadRequest

    val rep4 = Await.result(service(Request("/qux?foo=something")))
    rep4.contentString shouldBe "something"
    rep4.status shouldBe Status.Created
  }

  "A Value Endpoint" should "be convertible into a Service" in {
    val e: Endpoint[String] = get("foo") { Created("bar") }
    val s: Service[Request, Response] = e.toService

    val rep = Await.result(s(Request("/foo")))
    rep.contentString shouldBe "bar"
    rep.status shouldBe Status.Created

 */

  }
}
