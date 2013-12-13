case object A { override def toString = ??? }

object Test {
  def foo: Int = (A: Any) match {
    case 0 => 0
  }
  def main(args: Array[String]): Unit = {
    try {
      foo
      sys.error("no exception")
    } catch {
      case me: MatchError => assert(me.getMessage == "an instance of class A$", me.getMessage)
      case ex: Throwable => sys.error("not a match error: " + ex.getClass)
    }
  }
}
