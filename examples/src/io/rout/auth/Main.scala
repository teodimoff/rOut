package io.rout.example.auth


import com.twitter.app.Flag
import com.twitter.finagle.Http
import com.twitter.finagle.stats.{Counter, NullStatsReceiver}
import com.twitter.finagle.tracing.NullTracer
import com.twitter.server.TwitterServer
import com.twitter.util.{Await, Future}
import io.rout._
import io.routs._
import io.rout.generic.decoding._
import scala.util.Random

object Main extends TwitterServer {

  val port: Flag[Int] = flag("port", 8081, "TCP port for HTTP server")

  val passports: Counter = statsReceiver.counter("registered_passports")

  val todos: Counter = statsReceiver.counter("incomplete_todos")

  val passportDerive = derive[Passport].fromParams

  val derivedTodo: ReqRead[Todo] = derive[Int => Todo].incomplete.map(_(Random.nextInt(100000)))

  val getTodos = get(Root / "todos").filter[AuthedReq](r => Ok(Todo.list().mkString("\n")))

  val getTodo = get(Root / "todo" / Match[Int]).filter[AuthedReq]{ (auth, id) =>
    Todo.get(id) match {
      case Some(t) => Todo.delete(id); Ok(t.toString)
      case None => throw new TodoNotFound(id)
    }
  }

  val todo = post(Root / "todo").filter[AuthedReq](derivedTodo) { (auth, todo) =>
    todos.incr()
    Todo.save(todo)
    Created(s"User ${auth.passport.name.capitalize} Created -> ${todo.toString}")
  }

  val todoPath = post(Root / "todo" / Match[String]).filter[AuthedReq](derivedTodo) { (auth, string, todo) =>
    todos.incr()
    Todo.save(todo)
    Created(s"User ${auth.passport.name.toUpperCase} Created -> ${todo.toString}")
  }

  val regularTodo = post(Root / "todo" / "regular")(derivedTodo) { todo =>
    todos.incr()
    Todo.save(todo)
    Created(todo.toString)
  }

  val getRegistered = get(Root / "registered")(r => Ok(PassportDatabase.list().mkString("\n")))

  val register = post(Root / "register")(passportDerive){passport =>
    passports.incr()
    PassportDatabase.save(passport)
    Created(passport.toString)
  }

  val deleteRegistered = delete(Root / "registered") { req =>
    val all: List[Passport] = PassportDatabase.list()
    all.foreach(t => PassportDatabase.delete(t.password))
    Ok(all.mkString("\n"))
  }

  val rOut = mkRoutes(Seq(
    AuthFilter.auth andThen getTodo,
    AuthFilter.auth andThen getTodos,
    AuthFilter.auth andThen todo,
    AuthFilter.auth andThen todoPath,
    regularTodo,
    getRegistered,
    register,
    deleteRegistered
  ))
    .withNotFound("path was not found")
    .handle {
      case t: TodoNotFound => NotFound(t)
      case rua: RequestUnauthenticated => Forbidden(rua)
      case t: Throwable =>  Forbidden(new Exception(t))
    }


  def main(): Unit = {
    log.info("Serving the Todo Incompletes application (extract from request params)")

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
