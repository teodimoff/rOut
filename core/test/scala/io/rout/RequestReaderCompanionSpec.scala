package io.rout

import com.twitter.finagle.http.Request
import com.twitter.util.{Await, Future}
import io.rout.items._
import org.scalatest.{FlatSpec, Matchers}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ReqReadCompanionSpec extends FlatSpec with Matchers {

  "The ReqReadCompanion" should "support a factory method based on a function that reads from the request" in {
    val request: Request = Request(("foo", "5"))
    val futureResult: Future[Option[String]] = ReqRead[Option[String]](_ => Some("5"))(request)
    Await.result(futureResult) shouldBe Some("5")
  }

  it should "support a factory method based on a constant Future" in {
    val request: Request = Request(("foo", ""))
    val futureResult: Future[Int] = ReqRead.const(Future.value(1))(request)
    Await.result(futureResult) shouldBe 1
  }
  
  it should "support a factory method based on a constant value" in {
    val request: Request = Request(("foo", ""))
    val futureResult: Future[Int] = ReqRead.value(1)(request)
    Await.result(futureResult) shouldBe 1
  }
  
  it should "support a factory method based on a constant exception" in {
    val request: Request = Request(("foo", ""))
    val futureResult: Future[Int] = ReqRead.exception(Error.NotPresent(BodyItem))(request)
    an [Error.NotPresent] shouldBe thrownBy(Await.result(futureResult))
  }
  
}
