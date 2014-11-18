object Ex {
  def unapply(t: Throwable): Option[Throwable] = Some(t)
}

class A {
  var x = ""

  def bar =
    try {
      "bar"
    } finally {
      try {
        x += "a"
      } finally {
        x += "b"
        try {
          x += "c"
          throw null
        } catch {
          case Ex(_) =>
            x += "d"
        }
      }
    }
}

object Test extends App {
  val a = new A
  println(a.bar)
  println(a.x)
}
