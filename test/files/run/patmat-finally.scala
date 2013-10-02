/** Test pattern matching and finally, see SI-5929. */
object Test extends App {
  def bar(s1: Object, s2: Object) {
    s1 match {
      case _ =>
    }

    try {
      ()
    } finally {
      s2 match {
        case _ =>
      }
    }
  }

  def x = {
    null match { case _ => }

    try { 1 } finally { while(false) { } }
  }

  bar(null, null)
  x
}
