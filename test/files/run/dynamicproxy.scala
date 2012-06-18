
package p {
  import Test.F

  class A {
    override def toString = "A"
  }

  class B extends A {
    override def toString = "B"
  }

  class C extends B {
    def foo(x: A) = 1
    def foo(x: B) = 2
    def bar(x: A) = 3
    def bar(x: F) = 4
    override def toString = "C"
  }

  class D {
    def foz(x: A = new A) = x
    def baz(x: Int, y: A = new A) = (x, y)
    override def toString = "D"
  }
}

object Test extends App {
  import reflect.dynamic.DynamicProxy
  import p._

  class E extends A
  class F extends A

  class G {
    def biz(x: F) = x
  }

  val proxy1 = new DynamicProxy {
    val proxyTarget = new C
  }

  assert(proxy1.foo(new A) == 1)
  assert(proxy1.foo(new B) == 2)
  assert(proxy1.foo(new E) == 1)
  assert(proxy1.foo(new F) == 1)

  assert(proxy1.bar(new A) == 3)
  assert(proxy1.bar(new B) == 3)
  assert(proxy1.bar(new C) == 3)
  assert(proxy1.bar(new E) == 3)
  assert(proxy1.foo(new F) == 1)

  val proxy2 = new DynamicProxy {
    val proxyTarget = new D
  }

  assert(proxy2.foz().isInstanceOf[A])
  val a = new A
  assert(proxy2.foz(a) == a)
  val b = new B
  assert(proxy2.foz(b) == b)
  val (x1, y1) = proxy2.baz(1).asInstanceOf[(Int, A)]
  assert(x1 == 1)
  assert(y1.isInstanceOf[A])
  val (x2, y2) = proxy2.baz(2, a).asInstanceOf[(Int, A)]
  assert(x2 == 2)
  assert(y2.isInstanceOf[A])
  val (x3, y3) = proxy2.baz(2, b).asInstanceOf[(Int, A)]
  assert(x3 == 2)
  assert(y3.isInstanceOf[B])

  val proxy3 = new DynamicProxy {
    val proxyTarget = new G
  }

  val f = new F

  // Should work but won't due to SI-5944
  // assert(proxy3.biz(f) == f)

}
