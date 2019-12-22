// scalac: -Werror -Xlint
//
class A { def x: Int = 3 }
class B extends A { override def x(): Int = 4 }

class C extends java.lang.CharSequence {
  def charAt(x$1: Int): Char = ???
  def length: Int = ???
  def subSequence(x$1: Int, x$2: Int): CharSequence = ???
}
