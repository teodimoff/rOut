package io.rout

import scalatags.Text._
import scalatags.Text.all._

package object html {

  val bs = io.rout.html.BootstrapCss

  val tag = scalatags.Text.all

  def scalaTag[A](a: Seq[A]) = tag.a(a.map(e => li(e.toString)))

  def scalaTag[A](f :A => TypedTag[String]) = f

  def render(pageTitle: String,a: Seq[Modifier])(
    pageRender: (String,Seq[Modifier]) => TypedTag[String]): TypedTag[String] = pageRender(pageTitle,a)
}
