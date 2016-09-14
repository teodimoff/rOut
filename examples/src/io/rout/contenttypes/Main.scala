package io.rout.contenttypes

import com.twitter.app.Flag
import com.twitter.finagle.Http
import com.twitter.finagle.stats.{Counter, NullStatsReceiver}
import com.twitter.finagle.tracing.NullTracer
import com.twitter.server.TwitterServer
import com.twitter.util.Await
import io.rout._
import contentTypes._
import io.rout.generic.decoding._
import io.routs._
import io.rout.html._
import scalatags.Pretty.noPrettyPrint
import scalatags.Text.all._
import scala.util.Random

import io.circe.generic.auto._
import io.rout.circe._

object Main extends TwitterServer {

  //add multiple endpoints with different Content-types (and encoders/decoders added automatically)
  val port: Flag[Int] = flag("port", 8081, "TCP port for HTTP server")

  val passports: Counter = statsReceiver.counter("registered_passports")

  val todos: Counter = statsReceiver.counter("todos")

  val etodo = scalaTag[Todo](t => tag.a(t.productIterator.map(r=> tag.li(r.toString)).toSeq))

  implicit val encodeTodo: Encode.TextHtml[Todo] =
    Encode.scalaTag(t => etodo(t))

  implicit val seqTodo: Encode.TextHtml[Seq[Todo]]  =
    Encode.scalaTag(s => tag.a(s.map(etodo.apply)))

  val derivedTodo: ReqRead[Todo] = derive[Int => Todo].incomplete.map(_(Random.nextInt(100000)))

  val getTodos = get[Text.Html](Root / "todos").sync(r => Ok(Todo.list()))

  //this is awkward to specify matched int :(
  val getTodo = get[Int,Text.Html](Root / "todo" / Match[Int]).sync{ id =>
    Todo.get(id) match {
      case Some(t) => Ok(t)
      case None => throw new TodoNotFound(id)
    }
  }

  val htmljsonTodo =
    post[Application.Json](Root / "todo" / "json" / "html").sync(derivedTodo){ todo =>
      todos.incr()
      Todo.save(todo)
      Created(todo)
    }

  val jsonjsonTodo =
    post[Application.Json](Root / "todo" / "json" / "json").sync(binaryBody.asJson[Todo]){ todo =>
      todos.incr()
      Todo.save(todo)
      Created(todo)
    }

  val htmlTodo = post[Text.Html](Root / "todo").sync(derivedTodo) { todo =>
    todos.incr()
    Todo.save(todo)
    Created(todo)
  }

  val deleteTodo = delete(Root / Match[Int]).sync{id =>
    Todo.delete(id)
    Ok(s"todo $id deleted!")
  }

  val rOut = mkRoutes(Seq(
    getTodo,
    getTodos,
    htmlTodo,
    deleteTodo,
    htmljsonTodo,
    jsonjsonTodo))
    //.withNotFound("path was not found")
    .handle[Text.Html,Todo] {
      case t: TodoNotFound => NotFound(t)
      case t: Throwable =>  Forbidden(new Exception(t))
    }


  def main(): Unit = {
    log.info("Serving the Todo with multiple content-types (Text.Html,Application.Json)")

    val server = Http.server
      .withCompressionLevel(0)
      .withStatsReceiver(NullStatsReceiver)
      //.configured(Stats(statsReceiver))
      .withTracer(NullTracer)
      .serve(s":${port()}", rOut.service)

    onExit { server.close() }

    Await.ready(adminHttpServer)
  }
}
