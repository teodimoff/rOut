package io.rout.example.benchmark

import com.twitter.util.Time
import io.rout._
import io.routs._

object HelloWorld extends App {

  val helloWorld = get(Root).sync(
    Ok("Hello, World!")
      .withHeader("Server" -> "Example")
      .withHeader("Date" -> Time.now.toDate.toString)
  )

  serve(mkRoutes(Seq(helloWorld)).service)
}
