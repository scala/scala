
import language.implicitConversions

trait T
trait U {
  def u(x: Any) = x.toString * 2
}
class C {
  implicit def cv(t: T): U = new U {}
  def f(t: T): Any => String = cv(t).u _
  def g(t: T): Any => String = t.u _
  def h(t: T) = t.u _
}

object Test extends App {
  val c = new C
  val t = new T {}
  println {(
    c.f(t)("f"),
    c.g(t)("g"),
    c.h(t)("h"),
  )}
}

/*
2.11, 2.12 say:
t3218b.scala:11: error: _ must follow method; cannot follow Any => String
  def g(t: T): Any => String = t.u _
                                   ^
t3218b.scala:12: error: missing argument list for method u in trait U
Unapplied methods are only converted to functions when a function type is expected.
You can make this conversion explicit by writing `u _` or `u(_)` instead of `u`.
  def h(t: T) = t.u _
                    ^
*/
