
class X(val x: String)

class Y(y: => String) extends X(y) {
  // superaccessors used to munge it this way:
  //def f: String = Y.super.x.asInstanceOf[=> String]
  def f = y
}

object Y {
  def apply(y: => String): Y = new Y(y)
}

// sanity check

class V(x0: => String) { def x = x0 }

class W(y: => String) extends V(y) {
  def f = y
}

object W {
  def apply(y: => String): W = new W(y)
}

object Test extends App {
  // evaluated once on construction, then each invoke of f
  val y = Y {
    Console println "y..."
    "why"
  }
  Console println y.f
  Console println y.f
  val w = W {
    Console println "w..."
    "double-you"
  }
  Console println w.f
  Console println w.f
}
