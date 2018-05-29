package acyclicb

import pkg._

object CC {
  import DD.DD._

  val x = new X

  @doubler
  object C {
    class X { override def toString = "CX" }
  }
}
