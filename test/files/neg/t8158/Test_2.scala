import scala.language.experimental.macros

object X {
  def unapply(any: Any): Any = macro Max.impl
}

class BugTest {
  def bug(): Unit = {
    "any" match {
      case X() =>
      case _ => ???
    }
  }
}