package scalatags

import scalatags.Text.TypedTag

object Pretty {

  implicit class PrettyOps(val t: Text.TypedTag[String]) extends AnyVal{

    def prettyWriteTo(t: Text.TypedTag[String],strb: StringBuilder, depth: Int, step: Int): Unit = {
      val indent = " " * depth * step
      val builder = new text.Builder()
      t.build(builder)
      val attrs = builder.attrs.take(builder.attrIndex)
      val children = builder.children.take(builder.childIndex).toList

      def escape(s: String): String = {
        val sb = new StringBuilder
        Escaping.escape(s, sb)
        sb.toString()
      }

      strb ++= indent += '<' ++= t.tag

      strb ++= attrs.map(a => " " + a._1 + " = \"" + escape(a._2) + "\" ").mkString

      if (children.isEmpty && t.void) {
        strb ++= "/>"
      } else {
        strb ++= ">"
        for (c <- children) {
          c match {
            case t: TypedTag[String] =>
              strb ++= "\n"
              prettyWriteTo(t, strb, depth + 1, step)
            case any =>
              strb ++= "\n" ++= " " * (depth + 1) * step
              any.writeTo(strb)
          }
        }
        strb ++= "\n" ++= indent ++= "</" ++= t.tag += '>'
      }
    }

    def pretty(step: Int = 4): String = {
      val strb = new StringBuilder
      prettyWriteTo(t, strb, 0, step)
      strb.toString()
    }

    def pretty(): String = pretty(4)
  }

  implicit val prettyPrint = true

  implicit val noPrettyPrint = false
}
