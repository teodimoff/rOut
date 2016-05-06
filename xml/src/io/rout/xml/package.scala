package io.rout


package object xml extends Encoders with Decoders {
  object decode extends xml.Decoders

  object encode extends xml.Encoders
}

