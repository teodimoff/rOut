package io.rout.nested

import com.twitter.app.Flag
import com.twitter.finagle.Http
import com.twitter.finagle.http.filter.CorsFilter
import com.twitter.finagle.param.Stats
import com.twitter.finagle.stats.Counter
import com.twitter.server.TwitterServer
import com.twitter.util.{Await, Future}
import io.rout._
import io.routs._
import io.rout.generic.decoding._

import scala.util.Random

/*
 *for options you need to set Origin header and with Access-Control-Request-Method:POST or Get etc...
 *e.g (with httpie)
 * http options :8081/todo Origin:localhost:8081 Access-Control-Request-Method:POST --verbose
 *
 *OPTIONS /test?name=eee&age=4 HTTP/1.1
 *Accept: *//*
 *Accept-Encoding: gzip, deflate, compress
 *Access-Control-Request-Method: POST
 *Content-Length: 0
 *Host: localhost:8081
 *Origin: http://localhost:8081
 *User-Agent: HTTPie/0.8.0
 *
 *HTTP/1.1 200 OK
 *Access-Control-Allow-Headers: x-requested-with
 *Access-Control-Allow-Methods: POST
 *Access-Control-Allow-Origin: *
 *Content-Length: 0
 *Vary: Origin
 */
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

  val optionsTodo = options(Root / "todo")(o =>Ok("todo"))

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

  val postTodoRpc = post(Root / "todo" / "rpc")(rpcTodo) {todo =>
    todos.incr()
    Todo.save(todo)
    Created(todo.toString)
  }

  val postTodoPath = post(Root / "todo" / Match[Int])(derivedTodo){ (id,todo) =>
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

/*
  //path dont work if there are no options,Fix patchwithoptions.
  val patchTodo = derive[Todo].patch

  val patchedTodo = patch(Root / "todo" / Match[Int])(patchTodo){ (id,ftd) =>
    Todo.get(id) match {
      case Some(currentTodo) =>
        val newTodo: Todo = ftd(currentTodo)
        Todo.delete(id)
        Todo.save(newTodo)

        Ok(newTodo.toString)
      case None => throw TodoNotFound(id)
    }
  }
 */

  val rOut = mkRoutes(Seq(
    getTodo,
    getTodos,
    postTodo,
    postTodoRpc,
    postTodoPath,
    postTodoComplete,
    deleteTodos,
    deleteTodo,
    optionsTodo
    //patchedTodo
  ))
    .fileService()
    .asset("a",3)
    .debug
    .done
    .withNotFound("path was not found")

  def main(){
    log.info("Serving the Todo Incompletes application (extract from request params)")

    val server = Http.server
      .configured(Stats(statsReceiver))
      .serve(s":${port()}", CorsFilter("*","POST,GET") andThen rOut.service)

    onExit { server.close() }

    Await.ready(adminHttpServer)
  }
}
