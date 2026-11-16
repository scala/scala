// The qualifier reaches Predef through a rename.
import scala.{Predef => P}
import P.{assert}

class C { def m: Map[Int, Int] = null }
