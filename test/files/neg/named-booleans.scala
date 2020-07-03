// scalac: -Werror -Xlint:named-booleans

class C {
  def f(n: Int = 42, x: Boolean, y: Boolean) = if (x && y) n else 0

  def g(x: Any) =
    x match {
      case (true, false) => 0
      case _ => 1
    }
  var b = false
}

object Test extends App {
  val c = new C
  val b = false
  val x0 = c.f(17, true, b) // warn
  val x1 = c.f(17, x = true, b) // nowarn
  val x2 = c.f(y = b, n = 17, x = true) // nowarn
  c.b = true
  val y = Some(false)
  val z = Option(false)
  val w = (true, false)
  val v = c g true  // nowarn?

  val s = collection.mutable.Set.empty[String]
  def mutateS(): Unit = s("updater") = true
  //def updateS(): Unit = s.update("updater", true)

  val m = collection.mutable.Map.empty[String, true]
  def mutateM(): Unit = m("updater") = true

  def f(g: Boolean => Option[Boolean]) = g(true).getOrElse(false)
}

class Arrays {
  def test = Array(true, false, true)
}

class Tuples {
  def test = (true, false, true)
}

class Functions {
  val f: Boolean => Boolean = identity
  def test = f(true)
}
