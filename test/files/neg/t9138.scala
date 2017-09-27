//original bug

trait A
trait B extends A
abstract class C[T <: A] {
  def f(t: T)(s: String): T
}
  
class D extends C[B] {
  def f(b: B)(i: Int) = b
}

//minimal

trait Base[A] {
  def foo(a: A)(b: Int): Nothing
}

object Derived extends Base[String] {
  def foo(a: String): Nothing = ???
}

//expected behaviour

abstract class CCC[T <: A] {
  def f(t: T, s: String): T
}
  
class DDD extends CCC[B] {
  def f(b: Int, i: String) = b
}