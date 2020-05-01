package tastytest

object InlineOverrides:
  abstract class A:
    def f(x: Int) = s"dynamic $x"
    def h(x: Int): String
    inline def i(x: Int): String

  class B extends A:
    inline override def f(x: Int) = g(x)
    inline def g(x: Int) = s"inline $x"
    inline def h(x: Int) = g(x)
    inline def i(x: Int) = g(x)
