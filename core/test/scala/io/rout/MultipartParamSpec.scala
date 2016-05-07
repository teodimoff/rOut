package io.rout

import com.twitter.finagle.http.exp.Multipart
import com.twitter.finagle.http.{Request, RequestBuilder}
import com.twitter.util.{Await, Future}
import com.twitter.io.{Buf, Files}
import org.scalatest.{FlatSpec, Matchers}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
  * Specification for multipart/form-data request.
  *
  * The request is always a serialized request of a multipart upload
  * from Chrome.
  
  * The resource of the uploads.bytes was taken from:
  * https://github.com/twitter/finatra/blob/5d1d1cbb7640d8c4b1d11a85b53570d11a323e55/src/main/resources/upload.bytes
  * The file was created by using the following form:
  * <form enctype="multipart/form-data" action="/groups_file?debug=true" method="POST">
  *   <label for="groups">Filename:</label>
  *   <input type="file" name="groups" id="groups"><br>
  *   <input type="hidden" name="type" value="text"/>
  *   <input type="submit" name="submit" value="Submit">
  * </form>
  */
@RunWith(classOf[JUnitRunner])
class MultipartParamSpec extends FlatSpec with Matchers {

  "A RequiredMultipartFile" should "have a filename" in {
    val request = requestFromBinaryFile("/upload.bytes")
    val futureResult: Future[Multipart.FileUpload] = fileUpload("groups")(request)
    Await.result(futureResult).fileName shouldBe "dealwithit.gif"
  }

  it should "have a content type" in {
    val request = requestFromBinaryFile("/upload.bytes")
    val futureResult: Future[Multipart.FileUpload] = fileUpload("groups")(request)
    Await.result(futureResult).contentType shouldBe "image/gif"
  }

  it should "have a size greater zero" in {
    val request = requestFromBinaryFile("/upload.bytes")
    val futureResult: Future[Multipart.FileUpload] = fileUpload("groups")(request)
    val Multipart.OnDiskFileUpload(file, _, _, _) = Await.result(futureResult)
    val buf = Buf.ByteArray.Owned(Files.readBytes(file, limit = Int.MaxValue))
    buf.length should be > 0
  }

  it should "produce an error if the request body is not parsable as multipart" in {
    val request = RequestBuilder().url("http://example.com").buildPost(Buf.Utf8("&"))
    val futureResult = fileUpload("notmultipart")(request)
    an [Error.NotPresent] shouldBe thrownBy(Await.result(futureResult))
  }

  "An OptionalMultipartFile" should "have a filename if it exists" in {
    val request = requestFromBinaryFile("/upload.bytes")
    val futureResult: Future[Option[Multipart.FileUpload]] = fileUploadOption("groups")(request)
    Await.result(futureResult).get.fileName shouldBe "dealwithit.gif"
  }

  it should "be empty when the upload name exists but is not an upload" in {
    val request = RequestBuilder().url("http://localhost/").addFormElement("groups" -> "foo").buildFormPost()
    val futureResult: Future[Option[Multipart.FileUpload]] = fileUploadOption("groups")(request)
    Await.result(futureResult) shouldBe None
  }

  it should "produce an error if the request body is not parsable as multipart" in {
    val request = RequestBuilder().url("http://example.com").buildPost(Buf.Utf8("&"))
    val futureResult = fileUploadOption("notmultipart")(request)
    Await.result(futureResult) shouldBe None
  }

  "A RequiredMultipartParam" should "be properly parsed if it exists" in {
    val request = requestFromBinaryFile("/upload.bytes")
    val futureResult: Future[String] = param("type")(request)
    Await.result(futureResult) shouldBe "text"
  }

  it should "produce an error if the param does not exist" in {
    val request = requestFromBinaryFile("/upload.bytes")
    val futureResult: Future[String] = param("foo")(request)
    an [Error.NotPresent] shouldBe thrownBy(Await.result(futureResult))
  }

  it should "also return query parameters" in {
    val request = requestFromBinaryFile("/upload.bytes")
    val futureResult: Future[String] = param("debug")(request)
    Await.result(futureResult) shouldBe "true"
  }

  "An OptionalMultipartParam" should "be properly parsed when it exists" in {
    val request: Request = requestFromBinaryFile("/upload.bytes")
    val futureResult: Future[Option[String]] = paramOption("type")(request)
    Await.result(futureResult) shouldBe Some("text")
  }

  it should "produce an error if the param is empty" in {
    val request: Request = requestFromBinaryFile("/upload.bytes")
    val futureResult: Future[Option[String]] = paramOption("foo")(request)
    Await.result(futureResult) shouldBe None
  }

  it should "produce an error if the param name is present but not a param" in {
    val request: Request = requestFromBinaryFile("/upload.bytes")
    val futureResult: Future[Option[String]] = paramOption("groups")(request)
    Await.result(futureResult) shouldBe None
  }

  it should "allow to read both file uploads and attributes via request reader" in {
    val request: Request = requestFromBinaryFile("/upload.bytes")

    val paramFirst = (param("type") :: fileUpload("groups")).asTuple
    val (t1, g1) = Await.result(paramFirst(request))
    t1 shouldBe "text"
    g1.fileName shouldBe "dealwithit.gif"
    
    val fileFirst = (fileUpload("groups") :: param("type")).asTuple
    val (g2, t2) = Await.result(fileFirst(request))
    t2 shouldBe "text"
    g2.fileName shouldBe "dealwithit.gif"
  }

  private[this] def requestFromBinaryFile(resourceName: String): Request = {
    val s = getClass.getResourceAsStream(resourceName)
    val b = Stream.continually(s.read).takeWhile(_ != -1).map(_.toByte).toArray
    Request.decodeBytes(b)
  }
}
