//> using options -Werror -Xlint:overload

trait T

trait Base {
  def foo(implicit a: T) = 0
}

trait Derived1 extends Base {
  def foo = 0 // warn
}

trait Derived2 extends Base {
  val foo = 0 // warn
}

class C extends Base {
  private[this] val foo = 42 // warn
}

/* private local cannot directly conflict
class C2 extends Derived2 {
  private[this] val foo = 42 // weaker access privileges in overriding
}
*/

class Derived extends Derived2 // no warn, all foo are already members of Derived2

trait T1 {
  def foo = 0
}

class Mixed extends Base with T1 // warn here

class Inverted extends T1 {
  def foo(implicit a: T) = 0 // warn although x.foo picks this one
}

class D {
  def foo(a: List[Int])(implicit d: DummyImplicit) = 0
  def foo(a: List[String]) = 1
}

class CleverLukas {
  def foo(implicit e: String) = 1
  def foo(implicit e: Int) = 2
  val foo = 0 // warn
}

class MoreInspiration {
  def foo(implicit a: T) = 0
  def foo() = 1 // has parens but Scala 2 allows `foo` with adaptation
}

class X {
  val x = 42
}

class Y extends X {
  def x(implicit t: T) = 27 // warn
}

class J(val i: Int)
class K(i: Int) extends J(i) { // no warn local i shadows member i that is not implicit method
  def f = i
}

class Q {
  def i(implicit t: T) = 42
}
class R(val i: Int) extends Q // warn
class S(i: Int) extends R(i) { // warn
  def f = i
}

trait PBase {
  def f[A](implicit t: T) = 42
  def g[A](s: String) = s.toInt
}

trait PDerived extends PBase {
  def f[A] = 27 // warn
  def g[A] = f[A] // no warn
}

trait Matchers {
  def an[A: reflect.ClassTag] = ()
  val an = new Object // warn
}
object InnocentTest extends Matchers

trait T2 {
  @deprecated("old api", since="2.0")
  def foo = 0
}
object Evolved extends Base with T2 // no warn deprecated alternative, compare Mixed above

object Test extends App {
  implicit val t: T = new T {}
  val d1 = new Derived1 {} // no warn innocent client, already warned in evil parent
  println(d1.foo) // !
  val more = new MoreInspiration
  println(more.foo) // ?
  val y = new Y
  println(y.x) // you have been warned!
}
