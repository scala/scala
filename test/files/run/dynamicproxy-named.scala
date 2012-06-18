
import reflect.dynamic.DynamicProxy

class A {
  def foo(x: Int, y: Int) = x / y
  def bar(x: Int = 20, y: Int = 2) = x / y
  def /(? : Int, % : Int) = ? / %
}

object Test extends App {
  val proxy = new DynamicProxy {
    val proxyTarget = new A
  }

  assert(proxy.foo(10, 5) == 2)
  assert(proxy.foo(x = 10, y = 5) == 2)
  assert(proxy.foo(y = 5, x = 10) == 2)

  assert(proxy.bar() == 10)
  assert(proxy.bar(10) == 5)
  assert(proxy.bar(x = 10) == 5)
  assert(proxy.bar(10, 5) == 2)
  assert(proxy.bar(10, 5) == 2)
  assert(proxy.bar(x = 10, y = 5) == 2)
  assert(proxy.bar(y = 5, x = 10) == 2)
  assert(proxy.bar(y = 5) == 4)

  assert(proxy./(12, 4) == 3)
  assert(proxy./(12, % = 4) == 3)
  assert(proxy./(? = 12, % = 3) == 4)
  assert(proxy./(% = 20, ? = 40) == 2)

}
