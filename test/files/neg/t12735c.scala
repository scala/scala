//> using options -Werror -Xlint -Xreporter:scala.tools.partest.nest.PlainReporter

class UnusedVal {
  private val v: String =
    List(1) match {
      case Nil => "nil"
    }
}

class UnusedVals {
  private val v, w, x, y, z: String =
    List(1) match {
      case Nil => "nil"
    }
}

class UnusedIdents {
  private val v, w, x, y, z =
    List(1) match {
      case Nil => "nil"
    }
}

object UnusedAlias {
  private type int = Int
}
