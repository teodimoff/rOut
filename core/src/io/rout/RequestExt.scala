package io.rout

import com.twitter.finagle.http.Request

/**
  * Created by betepahos on 24.04.16.
  */
trait RequestExt[A] {
  def value: A
  def request: Request

}
