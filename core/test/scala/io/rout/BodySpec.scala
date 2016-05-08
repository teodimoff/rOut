package io.rout


import com.twitter.finagle.http.Request
import com.twitter.io.Buf
import com.twitter.util._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BodySpec extends RoutSpec {

  "A body reader" should "read the optional HTTP body as a string" in {
    check { req: Request =>
      val cs = req.contentString
      val bo = Await.result(bodyOption(req))
      (cs.isEmpty && bo === None) || (cs.nonEmpty && bo === Some(cs))
    }
  }

  it should "read the required HTTP body as a string" in {
    check { req: Request =>
      val cs = req.contentString
      val b = Await.result(body(req).liftToTry)
      (cs.isEmpty && b === Throw(Error.NotPresent(items.BodyItem))) ||
      (cs.nonEmpty && b === Return(cs))
    }
  }

  it should "read the optional HTTP body as a byte array" in {
    check { req: Request =>
      val cb = req.contentString.getBytes("UTF-8")
      val bo = Await.result(binaryBodyOptionArray(req))
      (cb.isEmpty && bo === None) || (cb.nonEmpty && bo.map(_.deep) === Some(cb.deep))
    }
  }

  it should "read the required HTTP body as a byte array" in {
    check { req: Request =>
      val cb = req.contentString.getBytes("UTF-8")
      val b = Await.result(binaryBodyArray(req).liftToTry)
      (cb.isEmpty && b === Throw(Error.NotPresent(items.BodyItem))) ||
      (cb.nonEmpty && b.map(_.deep) === Return(cb.deep))
    }
  }

  it should "has a corresponding request item" in {
    body.item shouldBe items.BodyItem
    bodyOption.item shouldBe items.BodyItem
    binaryBodyArray.item shouldBe items.BodyItem
    binaryBodyOptionArray.item shouldBe items.BodyItem
  }

  behavior of "param*"

  def withBody(b: String): Request = {
    val req = Request()
    val buf = Buf.Utf8(b)
    req.content = buf
    req.headerMap.put("Content-Length", buf.length.toString)

    req
  }

/*
  checkAll("Body[String]", EndpointLaws[String](bodyOption)(withBody).evaluating)
  checkAll("Body[Int]", EndpointLaws[Int](bodyOption)(withBody).evaluating)
  checkAll("Body[Long]", EndpointLaws[Long](bodyOption)(withBody).evaluating)
  checkAll("Body[Boolean]", EndpointLaws[Boolean](bodyOption)(withBody).evaluating)
  checkAll("Body[Float]", EndpointLaws[Float](bodyOption)(withBody).evaluating)
  checkAll("Body[Double]", EndpointLaws[Double](bodyOption)(withBody).evaluating)
  checkAll("Body[UUID]", EndpointLaws[UUID](bodyOption)(withBody).evaluating)
 */
}
