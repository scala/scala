//> using options -Wtrait-args -Werror

trait T

class C extends T() // error

class X
class Y extends X with T() // error

object funcs {
  def t: T = new T() {} // no error, permissive for Java anon syntax, just because
  def v: T = new X with T()  // error
  def w: T = new T {} // correct in every way
}
