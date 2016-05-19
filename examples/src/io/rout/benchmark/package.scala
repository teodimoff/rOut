package io.rout.example

import com.twitter.finagle.Http
import com.twitter.finagle.stats.NullStatsReceiver
import com.twitter.finagle.tracing.NullTracer
import com.twitter.util.Await
import io.routs._
import com.twitter.finagle.Service
import com.twitter.finagle.http.{Request,Response}

package object benchmark {

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
