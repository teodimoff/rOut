package io.rout

import com.twitter.finagle.http.Request

/**
  * RequestExt[A] which will be used for Filter that modify request chain i.e Filter[Request,Response,AuthRequest,Response]
  */
trait RequestExt[A] {
  def value: A
  def request: Request

}
