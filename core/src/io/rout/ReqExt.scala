package io.rout

import com.twitter.finagle.http.Request

/**
  * RequestExt[A] which will be used for Filter that modify request chain i.e
  * Filter[Request,Response,AuthRequest,Response]
  */
trait ReqExt[A] {self: A =>
  def value: A = self
  def request: Request
}

case class ReqReadExt[A](value: A,request: Request)