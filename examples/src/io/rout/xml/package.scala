package io.rout.example

import com.twitter.finagle.{Http, Service}
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.stats.NullStatsReceiver
import com.twitter.finagle.tracing.NullTracer
import com.twitter.util.Await
import io.routs._

package object xml {
  def serve(s: Service[Request,Response]) {
    log.info("Serving the Main application benchmark")

    val server = Http.server
      .withCompressionLevel(0)
      .withStatsReceiver(NullStatsReceiver)
      .withTracer(NullTracer)
      .serve(":8081", s)

    Await.ready(server)
  }
}
