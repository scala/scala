//> using options -Werror -Xlint -Xreporter:scala.tools.partest.nest.PlainReporter

class UnusedMethod {
  private def m: String =
    List(1) match {
      case Nil => "nil"
    }
}

object UnusedObject {
  private object X {
    def `stuff to create a range` = ()
  }
}
