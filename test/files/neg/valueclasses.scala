package test

trait T extends AnyVal // fail

class Foo {
  class Bar(x: Int) extends AnyVal // fail
  def foo() {
    class Baz(x: Int) extends AnyVal // fail
  }
}

class V1 extends AnyVal // fail

class V2(private[test] val x: Int) extends AnyVal // fail
class V3(protected[test] val x: Int) extends AnyVal // fail
class V4(protected val x: Int) extends AnyVal // fail
class V5(private val x: Int) extends AnyVal // fail

class V6(val x: Int, val y: String) extends AnyVal // fail
class V7(val x: Int, private[this] val y: String) extends AnyVal // fail
class V8(var x: Int) extends AnyVal // fail

class V9(val x: Int) extends AnyVal {
  val y = x    // fail
}

class V10[T](val x: T)  extends AnyVal // ok
class V11[T](val x: List[T]) extends AnyVal // ok
class V12[@specialized T, U](val x: (T, U)) extends AnyVal // fail

class V13(x: Int) extends AnyVal // fail

trait UT extends Any {
  var z: Int   // fail
  var zz = 123 // fail
}
class L(val t: Int) extends AnyVal
class ValueMembers(val t: Int) extends AnyVal {
  println(());
  { println(()) }
  55
  var x = 7
  x = 8

  type Foo = String
  def g1 = { lazy val x = t.toString ; x }
  def g2 = { lazy val x = t.toString ; () => x }
  def g3 = { def f(x: L) = x.t ; f _ }
  def g5(x: => L) = List(x, x)
  def g6(x: => L) = List(x, x) map (x => g3(x))
}
