import scala.language.implicitConversions

class Foo { type T = Int }

object Test {
  implicit def s2i(s: String): Int = s.length

  // The qualifiers of `q.T` and `q.type` are typed with implicit views disabled, in the block's
  // own context. `q`'s completer runs at that point, and must not inherit that mode.
  def typeSelection = { (??? : q.T); lazy val q = { val i: Int = "x"; new Foo }; q }
  def singletonType = { (??? : q.type); lazy val q = { val i: Int = "x"; new Foo }; q }

  // declaring `q` first was already fine
  def declaredFirst = { lazy val q = { val i: Int = "x"; new Foo }; (??? : q.T); q }
}
