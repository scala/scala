
import reflect.dynamic.DynamicProxy

class A {
  private def foo(x: Int) = x
}

object Test extends App {
  val p = new DynamicProxy {
    val proxyTarget = new A
    override val allowPrivate = true
  }
  assert(p.foo(1) == 1)
  assert(p.foo(2) == 2)
}
