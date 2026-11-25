// A top-level import masks Predef in the whole unit, including what precedes it.
class D { def m: Map[Int, Int] = null }

import scala.Predef.{assert}
