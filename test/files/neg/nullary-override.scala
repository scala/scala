// scalac: -Werror -Xlint:nullary-override
//
class A { def x: Int = 3 }
class B extends A { override def x(): Int = 4 }

class C extends java.lang.CharSequence {
  def charAt(x$1: Int): Char = ???
  def length: Int = ???
  def subSequence(x$1: Int, x$2: Int): CharSequence = ???
}

// P has parens
class P { def x(): Int = 3 }
// Q is questionable
class Q extends P { override def x: Int = 4 }

// Welcome to the Happy J
class J { override def toString = "Happy J" }
