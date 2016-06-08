package io.rout.routing


trait Routable { self =>
  val routes : Seq[RequestToService]
  lazy val className = self.getClass.getName
}
