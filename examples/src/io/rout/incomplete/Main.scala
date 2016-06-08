package io.rout.example.incomplete

import com.twitter.app.Flag
import com.twitter.finagle.Http
import com.twitter.finagle.param.Stats
import com.twitter.finagle.stats.Counter
import com.twitter.server.TwitterServer
import com.twitter.util.{Await, Future}
import io.rout._
import io.routs._
import io.rout.generic.decoding._
import io.rout.routing.NotFoundException

import scala.util.Random


object Main extends TwitterServer {

  val port: Flag[Int] = flag("port", 8081, "TCP port for HTTP server")

  val todos: Counter = statsReceiver.counter("incomplete_todos")

  def rpcId = Future(Random.nextInt(10000))

  val derivedTodo = derive[Int => Todo].incomplete

  val completeTodo = derive[Todo].fromParams

  val postedTodo: ReqRead[Todo] =
    derivedTodo.map(_(Random.nextInt(10000)))

  val rpcTodo: ReqRead[Future[Todo]] =
    derivedTodo.map(todo => rpcId.map(todo.apply))
      .filterf(todo =>
        todo.id > 0 &&
        todo.daysToComplete < 30 &&
        !todo.completed)

  val getTodo = get(Root / "todo" / Match[Int]) { id =>
    Todo.get(id) match {
      case Some(t) => Todo.delete(id); Ok(t.toString)
      case None => throw new TodoNotFound(id)
    }
  }

  val getTodos = get(Root / "todos")(r => Ok(Todo.list().mkString("\n")))

  val postTodo = post(Root / "todo")(postedTodo){todo =>
    todos.incr()
    Todo.save(todo)
    Created(todo.toString)
  }

  val postTodoRpc = post(Root / "todo" / "rpc")(rpcTodo){todo =>
    todos.incr()
    Todo.save(todo)
    Created(todo.toString)
  }

  val postTodoPath = post(Root / "todo" / Match[Int])(derivedTodo){ (id,todo)=>
    todos.incr()
    val t = todo(id)
    Todo.save(t)
    Created(t.toString)
  }

  val postTodoComplete = post(Root / "todo" / "complete")(completeTodo){todo =>
    todos.incr()
    Todo.save(todo)
    Created(todo.toString)
  }

  val deleteTodos = delete(Root / "todos") { req =>
    val all: List[Todo] = Todo.list()
    all.foreach(t => Todo.delete(t.id))
    Ok(all.mkString("\n"))
  }

  val deleteTodo = delete(Root / "todo" / Match[Int]) { id =>
    Todo.get(id) match {
      case Some(t) => Todo.delete(id); Ok(t.toString)
      case None => throw new TodoNotFound(id)
    }
  }

  val rOut = mkRoutes(Seq(
    getTodo,
    getTodos,
    postTodo,
    postTodoRpc,
    postTodoPath,
    postTodoComplete,
    deleteTodos,
    deleteTodo
  ))
    .fileService()
    .asset("a",3)
    .debug
    .done
    .handle {
      case NotFoundException => NotFound(new Exception("path was not found"))
      case e: Exception => NotFound(e)
    }

  def main(): Unit = {
    log.info("Serving the Todo Incompletes application (extract from request params)")

    val server = Http.server
      .configured(Stats(statsReceiver))
      .serve(s":${port()}", rOut.service)

    onExit { server.close() }

    Await.ready(adminHttpServer)
  }
}
