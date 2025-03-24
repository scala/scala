//> using options -Xsource:3 -Werror

trait A {
  def f: AnyRef
}

class C {
  def a = Option(new { def g = 1 }) // warn
  def b: Option[{ def g: Int }] = Option(new { def g = 1 }) // ok

  def c(p: { def i: Int }): Int = 0 // ok
  def d = new A { def f: A = this } // ok

  def e = new A { def f: AnyRef = new AnyRef } // ok
  def f = new A { def f = new AnyRef } // ok
  def g = new A { def f = this } // warn -- inferred type of `f` is `A`, since we're not using -Xsource-features:infer-override

  def h = new AnyRef { type T = String } // ok
  def i = new AnyRef { val x = 2 } // warn
}
