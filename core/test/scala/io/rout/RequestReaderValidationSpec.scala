package io.rout

import com.twitter.finagle.http.Request
import com.twitter.util.{Await, Future, Return, Throw}
import org.scalacheck.Prop.BooleanOperators
import org.scalatest.prop.Checkers
import org.scalatest.{FlatSpec, Matchers}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ReqReadValidationSpec extends FlatSpec with Matchers with Checkers {

  val request = Request("foo" -> "6", "bar" -> "9")
  val fooReader = param("foo").asText[Int]
  val barReader = param("bar").asText[Int]

  val beEven = ValidationRule[Int]("be even") { _ % 2 == 0 }
  def beSmallerThan(value: Int) = ValidationRule[Int](s"be smaller than $value") { _ < value }

  "A ReqRead" should "allow valid values" in {
    val evenReader = fooReader.should("be even") { _ % 2 == 0 }
    Await.result(evenReader(request)) shouldBe 6
  }
  
  it should "allow valid values based on negated rules" in {
    val evenReader = barReader.shouldNot("be even") { _ % 2 == 0 }
    Await.result(evenReader(request)) shouldBe 9
  }

  it should "raise a ReqRead error for invalid values" in {
    val oddReader = fooReader.should("be odd") { _ % 2 != 0 }
    an [Error] shouldBe thrownBy(Await.result(oddReader(request)))
  }

  it should "be lift-able into an optional reader that always succeeds" in {
    val oddReader = fooReader.should("be odd") { _ % 2 != 0 }
    Await.result(oddReader.lift(request)) shouldBe None
  }

  it should "allow valid values in a for-comprehension" in {
    val readFoo: ReqRead[Int] = for {
      foo <- fooReader if foo % 2 == 0
    } yield foo
    Await.result(readFoo(request)) shouldBe 6
  }

  it should "raise a ReqRead error for invalid values in a for-comprehension" in {
    val readFoo: ReqRead[Int] = for {
      foo <- fooReader if foo % 2 != 0
    } yield foo
    an [Error] shouldBe thrownBy(Await.result(readFoo(request)))
  }

  it should "be convertible to a single-member case class with a matching type" in {
    case class Bar(i: Int)
    val barReader = fooReader.as[Bar]

    Await.result(barReader(request)) shouldBe Bar(6)
  }

  "A ReqRead with a predefined validation rule" should "allow valid values" in {
    val evenReader = fooReader.should(beEven)
    Await.result(evenReader(request)) shouldBe 6
  }
  
  it should "allow valid values based on negated rules" in {
    val evenReader = barReader.shouldNot(beEven)
    Await.result(evenReader(request)) shouldBe 9
  }

  it should "raise a ReqRead error for invalid values" in {
    val oddReader = fooReader.shouldNot(beEven)
    an [Error] shouldBe thrownBy(Await.result(oddReader(request)))
  }
  
  it should "allow valid values based on two rules combined with and" in {
    val andReader = fooReader.should(beEven and beSmallerThan(7))
    Await.result(andReader(request)) shouldBe 6
  }
  
  it should "raise a ReqRead error if one of two rules combined with and fails" in {
    val andReader = fooReader.should(beEven and beSmallerThan(2))
    an [Error] shouldBe thrownBy(Await.result(andReader(request)))
  }
  
  it should "allow valid values based on two rules combined with or" in {
    val orReader = barReader.shouldNot(beEven or beSmallerThan(2))
    Await.result(orReader(request)) shouldBe 9
  }
  
  it should "raise a ReqRead error if one of two rules combined with or in a negation fails" in {
    val andReader = fooReader.shouldNot(beEven or beSmallerThan(12))
    an [Error] shouldBe thrownBy(Await.result(andReader(request)))
  }
  
  it should "allow to reuse a validation rule with optional readers" in {
    val optReader = paramOption("foo").asText[Int].should(beEven)
    Await.result(optReader(request)) shouldBe Some(6)
  }
  
  it should "raise a ReqRead error if a rule for a non-empty optional value fails" in {
    val optReader = paramOption("bar").asText[Int].should(beEven)
    an [Error] shouldBe thrownBy(Await.result(optReader(request)))
  }
  
  it should "skip validation when applied to an empty optional value" in {
    val optReader = paramOption("baz").asText[Int].should(beEven)
    Await.result(optReader(request)) shouldBe None
  }

  it should "work with predefined rules" in {
    val intReader = param("foo").asText[Int] should beGreaterThan(100)
    val floatReader = param("bar").asText[Float].should(beGreaterThan(100.0f))
    val stringReader = param("baz").should(beLongerThan(10))
    val optLongReader = paramOption("foo").asText[Int] should beGreaterThan(100)

    val ltIntReader = param("foo").asText[Int] should beLessThan(100)
    val stStringReader = param("baz").should(beShorterThan(10))

    val req = Request("foo" -> "20", "bar" -> "20.0", "baz" -> "baz")

    an [Error] shouldBe thrownBy(Await.result(intReader(req)))
    an [Error] shouldBe thrownBy(Await.result(floatReader(req)))
    an [Error] shouldBe thrownBy(Await.result(stringReader(req)))
    an [Error] shouldBe thrownBy(Await.result(optLongReader(req)))

    Await.result(ltIntReader(req)) shouldBe 20
    Await.result(stStringReader(req)) shouldBe "baz"
  }

  it should "allow to use inline rules with optional params" in {
    val optInt = paramOption("foo").asText[Int].should("be greater than 50") { i: Int => i > 50 }
    val optString = paramOption("bar").should("be longer than 5 chars") { s: String => s.length > 5 }

    an [Error] shouldBe thrownBy(Await.result(optInt(request)))
    an [Error] shouldBe thrownBy(Await.result(optString(request)))
  }

  "An empty optional param ReqRead" should "work correctly with inline rules" in {
    val optInt = paramOption("baz").asText[Int].should("be greater than 50") { i: Int => i > 50 }

    Await.result(optInt(request)) shouldBe None
  }

  "A composite ReqRead" should "be convertible to an appropriately typed case class" in {
    case class Qux(i: Int, j: Int)
    val quxReader = (fooReader :: barReader).as[Qux]

    Await.result(quxReader(request)) shouldBe Qux(6, 9)
  }

  it should "be convertible to a tuple" in {
    val tupleReader = (fooReader :: barReader).asTuple

    Await.result(tupleReader(request)).shouldBe((6, 9))
  }

  it should "correctly fail with a single error" in {
    val firstBadReader = (fooReader.shouldNot(beEven) :: barReader.shouldNot(beEven)).asTuple
    val secondBadReader = (fooReader.should(beEven) :: barReader.should(beEven)).asTuple

    Await.result(firstBadReader(request).liftToTry) should matchPattern {
      case Throw(Error.NotValid(_, _)) =>
    }

    Await.result(secondBadReader(request).liftToTry) should matchPattern {
      case Throw(Error.NotValid(_, _)) =>
    }
  }

  it should "correctly accumulate errors" in {
    val tupleReader = (fooReader.shouldNot(beEven) :: barReader.should(beEven)).asTuple

    Await.result(tupleReader(request).liftToTry) should matchPattern {
      case Throw(Error.RequestErrors(Seq(Error.NotValid(_, _), Error.NotValid(_, _)))) =>
    }
  }

  it should "be able to have a function with appropriate arity and types applied to it" in {
    val add: (Int, Int) => Int = _ + _
    val sumReader = (fooReader :: barReader) ~> add

    check { (foo: Int, bar: Int) =>
      val req = Request("foo" -> foo.toString, "bar" -> bar.toString)

      Await.result(sumReader(req)) === foo + bar
    }
  }

  it should "be able to have an appropriately-typed Future-returning function applied to it" in {
    val div: (Int, Int) => Future[Int] = (x, y) => Future(x / y)
    val divReader = (fooReader :: barReader) ~~> div

    check { (foo: Int, bar: Int) =>
      val req = Request("foo" -> foo.toString, "bar" -> bar.toString)

      Await.result {
        for {
          result <- div(foo, bar).liftToTry
          readResult <- divReader(req).liftToTry
        } yield (readResult, result) match {
          case (Return(r1), Return(r2)) => r1 == r2
          case (Throw(e1), Throw(e2)) => e1.getMessage == e2.getMessage
          case _ => false
        }
      }
    }
  }

/*
  "ReqRead's generic derivation" should "create valid readers for case classes" in {
    case class User(id: Long, first: String, last: String)

    val userReader = io.rout.generic.decoding.derive[User].fromParams

    check { (id: Long, first: String, last: String) =>
      (first.nonEmpty && last.nonEmpty) ==> {
        val req = Request("id" -> id.toString, "first" -> first, "last" -> last)
        Await.result(userReader(req)) === User(id, first, last)
      }
    }
  }
 */
}
