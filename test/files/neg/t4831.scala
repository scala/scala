object O {
  val a = 0
  val b = 1
}

import O.{a => b}
import O.b

object test {
  println(b)
}
