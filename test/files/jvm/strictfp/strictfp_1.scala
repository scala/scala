package strictfp

import annotation.strictfp

class A {
  @strictfp def foo(f: Float) = { def bar = f ; bar }
}

trait B {
  @strictfp def foo(f: Float) = { def bar = f ; bar }
}

@strictfp class C {
  def foo(f: Float) = { def bar = f; bar }
}

@strictfp trait D {
  def foo(f: Float) = { def bar = f; bar }
}

class E(val f: Float) extends AnyVal {
  @strictfp def foo = { def bar = f; bar }
}

@strictfp class F(val f: Float) extends AnyVal {
  def foo = { def bar = f; bar }
}

@strictfp class G {
  class I  { def foo(f: Float) = { def bar = f; bar } }
  object I { def foo(f: Float) = { def bar = f; bar } }
}

@strictfp object H {
  class I  { def foo(f: Float) = { def bar = f; bar } }
  object I { def foo(f: Float) = { def bar = f; bar } }
}

class I(val f: Float) extends AnyVal {
  def foo = { // NO
    def bar = f // NO
    bar
  }
}
@strictfp object I {
  def foo(f: Float) = {
    def bar = f
    bar
  }
}

@strictfp class J {
  def foo = {
    class M {
      def foo(f: Float) = {
        def bar = f
        bar
      }
    }
    new M
  }
}

class K {
  @strictfp def foo = {
    class M {
      def foo(f: Float) = {
        def bar = f
        bar
      }
    }
    new M
  }
}

// see run/t8574.scala for interaction with @specialized