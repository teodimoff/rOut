package io.rout

import java.util.UUID

import algebra.Eq
import cats.Show
import com.twitter.io.Buf
import com.twitter.util.{Return, Throw, Try}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Type class instances for non-Finch types.
 */
@RunWith(classOf[JUnitRunner])
trait MissingInstances {
  implicit def eqTry[A](implicit A: Eq[A]): Eq[Try[A]] = Eq.instance {
    case (Return(a), Return(b)) => A.eqv(a, b)
    case (Throw(x), Throw(y)) => x == y
    case _ => false
  }

  implicit def eqUUID: Eq[UUID] = Eq.fromUniversalEquals

  implicit def showUUID: Show[UUID] = Show.fromToString

  implicit def eqBuf: Eq[Buf] = Eq.fromUniversalEquals
}
