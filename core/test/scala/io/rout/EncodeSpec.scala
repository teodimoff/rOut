package io.rout

import java.util.UUID
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EncodeSpec extends RoutSpec {
  checkAll("Encode.TextHtml[String]", EncodeLaws.texthtml[String].all)
  checkAll("Encode.TextHtml[Int]", EncodeLaws.texthtml[Int].all)
  checkAll("Encode.TextHtml[Option[Boolean]]", EncodeLaws.texthtml[Option[Boolean]].all)
  checkAll("Encode.TextHtml[List[Long]]", EncodeLaws.texthtml[List[Long]].all)
  checkAll("Encode.TextHtml[Either[UUID, Float]]", EncodeLaws.texthtml[Either[UUID, Float]].all)
}
