//> using options -Werror -Wunnamed-boolean-literal

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
  val x0 = c.f(17, true, false) // warn
  val x1 = c.f(17, true, b) // nowarn
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

case class Klazz(isKlazz: Boolean, isWarnable: Boolean)

class Klazzy {
  def test = Klazz(true, false) // warn case class apply as for ctor
}

class Defaulting {
  def f(n: Int, up: Boolean = true, down: Boolean = false) = if (up) n+1 else if (down) n-1 else n
  def g0 = f(42) // nowarn, all defaults
  def g1 = f(42, up=false) // nowarn, named or defaults
  def g2 = f(42, up=false, true) // nowarn, in param order so not a named block, unnamed is last remaining param
  def g3 = f(42, false) // warn, unnamed could mean either param with default
  def g4 = f(42, false, true) // warn, swappable

  def rev(n: Int, reverse: Boolean = false, up: Boolean = true, down: Boolean = false) =
    if (!reverse) f(n, up, down) else if (down) n+1 else if (up) n-1 else n
  def rev0 = rev(42) // nowarn, all defaults
  def rev1 = rev(42, up=false) // nowarn, named or defaults
  def rev2 = rev(42, true, up=false, down=true) // nowarn, in param order so not a named block, unnamed is last remaining param
  def rev3 = rev(42, reverse=true, false) // warn, unnamed could mean either param with default
  def rev4 = rev(42, false, true, false) // warn, swappable
  def rev5 = rev(42, true, down=true) // warn, out of order so it's a named block, otherwise same as rev3
}
