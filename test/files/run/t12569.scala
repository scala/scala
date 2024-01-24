
final class C[A, B](pf: PartialFunction[A, B]) {
  def f(x: A) =
    pf.applyOrElse(x, C.fallback) match {
      case _: C.fallback.type => -1
      case _ => 42
    }
}
object C {
  val fallback: Any => Any = _ => fallback
}

object Test extends App {
  val c = new C[String, Int] ({ case "default" => 0 case s if s.length < 4 => Integer.parseInt(s) })
  assert(c.f("5") == 42)
  assert(c.f("default") == 42)
  assert(c.f("junk") == -1)
}
