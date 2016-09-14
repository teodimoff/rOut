package io.rout.contenttypes

import scala.collection.mutable


case class Todo(id: Int, title: String, completed: Boolean, order: Option[Int], advisor: Option[String])

object Todo {
  private[this] val db: mutable.Map[Int, Todo] = mutable.Map.empty[Int, Todo]

  def get(id: Int): Option[Todo] = synchronized { db.get(id) }
  def list(): Seq[Todo] = synchronized { db.values.toSeq }
  def save(t: Todo): Unit = synchronized { db += (t.id -> t) }
  def delete(id: Int): Unit = synchronized { db -= id }
}

case class TodoNotFound(id: Int) extends Exception {
  override def getMessage: String = s"Todo(${id.toString}) not found."
}