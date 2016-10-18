package p1

sealed trait Base

object Test {
  val x = Macro.p1_Base_knownDirectSubclasses
}

case class B(val b: Test.x.type)
