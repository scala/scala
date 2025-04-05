// status quo

trait T

class C extends T() // ok
class D extends T(42) // error

class X
class Y extends X with T() // ok
class Z extends X with T(42) // error

object funcs {
  def t: T = new T() {} // no error, permissive for Java anon syntax, just because
  def u: T = new T(42) {} // error
  def v: T = new X with T() // ok
  def w: T = new X with T(42) // error
}
