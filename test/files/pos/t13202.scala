// scala/bug#13202
// A top-level lub that forces a lazy type completer can be re-entered by a lub in the
// completed code. The nested lub must not see the outer lub's recursion guard (`Any`),
// nor clear the outer's caches, otherwise inferred types depend on typechecking order.
object Test { def g(b: Boolean) = if (b) new B else new C } // lub(B, C) forces B.f
trait P { def f: Any }
class B extends P { def f = { val x = if (true) new B else new C; x } }
class C extends P { def f = { val x = if (true) new B else new C; x } }

object Check {
  val b: P = (new B).f
  val c: P = (new C).f
  val g: P = Test.g(true).f
}
