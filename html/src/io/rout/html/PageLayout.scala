package io.rout.html

import scalatags.Text._
import scalatags.Text.all._
import scalatags.Text.tags2.title

object PageLayout {

  val bs = BootstrapCss

  val tag = scalatags.Text.all

  private def expandName(names: Seq[String],expand: String) = names map(name => s"/a/$expand/$name")

  def css(names: Seq[String] = Nil)=
    if(names == Nil) Seq.empty[Modifier]
  else expandName(names,"css") map(sheet => link(href:= sheet, rel:="stylesheet"))

  def js(names: Seq[String] = Nil) =
    if(names == Nil) Seq.empty[Modifier]
  else expandName(names,"js") map(name => script(tpe:="text/javascript", src:=name))

  def page(titleName:String,content: Seq[Modifier],jsScripts: Seq[String]= Nil,cssScripts: Seq[String] = Nil) =
    html(head(title(titleName), css(cssScripts)), body(content),js(jsScripts))

  def pageScripts(titleName:String,
           content: Seq[Modifier],
           jsScripts: Seq[Modifier]= Seq.empty[Modifier],
           cssScripts: Seq[Modifier] = Seq.empty[Modifier]) =
    html(head(title(titleName), cssScripts), body(content),jsScripts)

  def scalaTag[A](f:A => TypedTag[String]) = f

}
