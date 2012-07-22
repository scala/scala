class A {
  def foo1(x: Int*) = ???
  def foo2(x: => Int) = ???
  def foo3(x: Any) = ???
  def foo4(x: AnyRef) = ???
  def foo5(x: AnyVal) = ???
  def foo6(x: Null) = ???
  def foo7(x: Nothing) = ???
  def foo8(x: Singleton) = ???
}

object Test extends App {
  import scala.reflect.runtime.universe._
  def test(n: Int): Unit = {
    val sig = typeOf[A] member newTermName("foo" + n) typeSignature
    val x = sig.asInstanceOf[MethodType].params.head
    println(x.typeSignature)
  }
  for (i <- 1 to 8) test(i)
}
