//> using options -Werror -Wunnamed-boolean-literal-strict

class C {
  def f(n: Int = 42, x: Boolean, y: Boolean) = if (x && y) n else 0

  def g(x: Any) =
    x match {
      case (true, false) => 0
      case _ => 1
    }
  var b = false
  def fs(n: Int)(s: String, b: Boolean) = if (b) s*n else s
  def gs[A](n: Int)(s: A, b: Boolean) = if (b) s.toString*n else s.toString

  def check(cond: Boolean, msg: => String) = if (cond) println(msg)
  def uncheck(cond: Boolean, msg: => String, flag: Boolean) = if (cond && flag) println(msg)
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
  val v = c g true  // nowarn infix

  val s = collection.mutable.Set.empty[String]
  def mutateS(): Unit = s("updater") = true
  //def updateS(): Unit = s.update("updater", true)

  val m = collection.mutable.Map.empty[String, true]
  def mutateM(): Unit = m("updater") = true

  val ss = c.fs(42)("hello", true)
  val tt = c.gs(42)("hello", true)

  def f(g: Boolean => Option[Boolean]) = g(true).getOrElse(false)

  c.check(true, "OK")
  c.uncheck(false, "OK", true)
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

case class Klazz(isKlazz: Boolean)

class Klazzy {
  def test = Klazz(true) // nowarn case class apply as for ctor
}

class Defaulting {
  def f(n: Int, up: Boolean = true, down: Boolean = false) = if (up) n+1 else if (down) n-1 else n
  def g0 = f(42)
  def g1 = f(42, up=false)
  def g2 = f(42, up=false, true)
  def g3 = f(42, false)
  def g4 = f(42, false, true)
}
