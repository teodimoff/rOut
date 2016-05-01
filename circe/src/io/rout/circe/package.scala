package io.rout

import io.circe.{Json, Printer}
import io.circe.jackson._

package object circe extends Encoders with Decoders{

    protected def print(json: Json): String = Printer.noSpaces.pretty(json)

    object dropNullKeys extends Encoders with Decoders{
      private val printer: Printer = Printer.noSpaces.copy(dropNullKeys = true)
      protected def print(json: Json): String = printer.pretty(json)
    }

   object jacksonSerializer extends Encoders with Decoders{
     protected def print(json: Json): String = jacksonPrint(json)
   }
}
