
import reflect.dynamic.DynamicProxy

class A {
  var foo = 1
  var % = 2
}

object Test extends App {
  val proxy = new DynamicProxy {
    val proxyTarget = new A
  }

  assert(proxy.foo == 1)
  proxy.foo = 3
  assert(proxy.foo == 3)
  proxy.foo = 7 + proxy.foo.asInstanceOf[Int]
  assert(proxy.foo == 10)

  assert(proxy.% == 2)
  proxy.% = 7
  assert(proxy.% == 7)
  proxy.% = 1 + proxy.%.asInstanceOf[Int]
  assert(proxy.% == 8)
}
