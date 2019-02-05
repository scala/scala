package test


import m.Macros._
import m._

// this case class should use the encodeHList implicit macro
case class Bar(x: Int) extends HList

class Foo {
  // this tests that the encoder works even though there are two possible implicit macros,
  // only one of which satisfies the type bounds. We're interested to see this type-check,
  // instead of seeing an ambiguous implicit error
  val encoder = Auto.deriveEncoder[Bar]
}
