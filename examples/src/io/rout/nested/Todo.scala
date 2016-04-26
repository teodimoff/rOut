package io.rout.nested

import scala.collection.mutable

/**
  * Created by betepahos on 25.04.16.
  */
case class Todo(id: Int, title: String, completed: Boolean, order: Option[Int], extraInfo: ExtraInfo)

case class ExtraInfo(daysToComplete: Int,relatedTodos: Option[Seq[Int]], advisor: Option[String])

object Todo {
  private[this] val db: mutable.Map[Int, Todo] = mutable.Map.empty[Int, Todo]

  def get(id: Int): Option[Todo] = synchronized { db.get(id) }
  def list(): List[Todo] = synchronized { db.values.toList }
  def save(t: Todo): Unit = synchronized { db += (t.id -> t) }
  def delete(id: Int): Unit = synchronized { db -= id }
}

case class TodoNotFound(id: Int) extends Exception {
  override def getMessage: String = s"Todo(${id.toString}) not found."
}