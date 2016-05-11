package io.rout.routing

import com.twitter.finagle.Filter
import com.twitter.finagle.http.{Request,Response}


trait Routable { self =>
  val routes : Seq[RequestToService]
  lazy val className = self.getClass.getName
}
