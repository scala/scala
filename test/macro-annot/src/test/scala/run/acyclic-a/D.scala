package acyclica

import C._

class CX extends X

@pkg.pretty
object D {
  class X { override def toString = "DX" }
}
