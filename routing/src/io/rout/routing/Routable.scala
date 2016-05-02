package io.rout.routing

/**
 * Created by betepahos on 24.03.16.
 */

trait Routable { self =>
  val routes : Seq[RequestToService]
  lazy val className = self.getClass.getName
}
