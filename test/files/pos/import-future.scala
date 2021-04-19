// scalac: -Xsource:3
//

import java.io as jio
import scala.{collection as c}

import c.mutable as mut
import mut.ArrayBuffer as Buf

object O {
  val x: jio.IOException = ???
  val y = Buf(1, 2, 3)

  type OString = String
  def foo22(x: Int) = x
}

class C {
  import O.{ foo22 as foo, OString as OS }
  println(foo(22))
  val s: OS = ""

  import mut.*
  val ab = ArrayBuffer(1)
}
