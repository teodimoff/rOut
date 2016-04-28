package io.rout.auth

import com.twitter.app.Flag
import com.twitter.finagle.Http
import com.twitter.finagle.param.Stats
import com.twitter.finagle.stats.Counter
import com.twitter.server.TwitterServer
import com.twitter.util.{Await, Future}
import io.rout._
import io.routs._
import io.rout.generic.decoding._
import rOut.examples.src.io.rout.auth.{AuthFilter, AuthedReq, Passport, PassportDatabase}

import scala.util.Random


object Main extends TwitterServer {

  val port: Flag[Int] = flag("port", 8081, "TCP port for HTTP server")

  val passports: Counter = statsReceiver.counter("registered_passports")

  val todos: Counter = statsReceiver.counter("incomplete_todos")

  val passportDerive = derive[Passport].fromParams

  val derivedTodo: ReqRead[Todo] = derive[Int => Todo].incomplete.map(_(Random.nextInt(10000)))

  val getRegistered = get(Root / "registered")(r => Ok(PassportDatabase.list().mkString("\n")))

  val register = post(Root / "register")(passportDerive){passport =>
    passports.incr()
    PassportDatabase.save(passport)
    Created(passport.toString)
  }

  val authedTodo = post[AuthedReq](Root / "todo").filter(derivedTodo) { (auth, todo) =>
    todos.incr()
    Todo.save(todo)
    Created(s"User ${auth.passport.name.toUpperCase} Created -> ${todo.toString}")
  }
  //specifying path return type is awkward. can we make things better?
  val authedTodoPath = post[AuthedReq,String](Root / "todo" / Match[String]).filter(derivedTodo) { (auth,string,todo) =>
    todos.incr()
    Todo.save(todo)
    Created(s"User ${auth.passport.name.toUpperCase} Created -> ${todo.toString}")
  }

  val regularTodo = post(Root / "todo" / "regular")(derivedTodo) { todo =>
    todos.incr()
    Todo.save(todo)
    Created(todo.toString)
  }

  val deleteRegistered = delete(Root / "registered") { req =>
    val all: List[Passport] = PassportDatabase.list()
    all.foreach(t => PassportDatabase.delete(t.password))
    Ok(all.mkString("\n"))
  }

  val rOut = mkRoutes(Seq(
    AuthFilter.auth andThen authedTodo,
    AuthFilter.auth andThen authedTodoPath,
    regularTodo,
    getRegistered,
    register,
    deleteRegistered

  ))
    .withNotFound("path was not found")

  def main(): Unit = {
    log.info("Serving the Todo Incompletes application (extract from request params)")

    val server = Http.server
      .configured(Stats(statsReceiver))
      .serve(s":${port()}", rOut.service)

    onExit { server.close() }

    Await.ready(adminHttpServer)
  }
}
