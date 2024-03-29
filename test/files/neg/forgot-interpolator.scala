//> using options -Werror -Xlint:missing-interpolator

class A {
  val bippy = 123

  def f = "Put the $bippy in the $bippy!" // warn 1
}

class B {
  val dingus = 123

  def f = "Put the $bippy in the $bippy!" // no warn
}

class C {
  def f = """Put the ${println("bippy")} in the bippy!""" // warn 2
}

package object test {
  def aleppo = 9
  def greppo(n: Int) = ???
  def zappos(n: Int)(implicit ord: math.Ordering[Int]) = ???
  def hippo(implicit n: Int) = ???
}

package test {
  // not sure if overloading is kosher in pkg obj yet
  class Doo {
    def beppo(i: Int) = 8 * i
    def beppo = 8
    class Dah extends Doo {
      def f = "$beppo was a marx bros who saw dollars."  // warn 3
    }
  }
  class E {
    def f = "$aleppo is a pepper and a city."     // warn 4
    def k = s"Just an interpolation of $aleppo"   // no warn
  }
  class Bar {
    private def bar = 8
    if (bar > 8) ???       // use it to avoid extra warning
  }
  class Baz extends Bar {
    def f = "$bar is private, shall we warn just in case?" // no longer a warning, private members aren't inherited!
  }
  class G {
    def g = "$greppo takes an arg"  // no warn
    def z = "$zappos takes an arg too"  // no warn
    def h = "$hippo takes an implicit"  // warn 6
  }
  class J {
    def j = 8
    class J2 {
      def j(i: Int) = 2 * i
      def jj = "shadowed $j"  // no warn
    }
  }
  import annotation._
  @implicitNotFound("No Z in ${T}")   // no warn
  class Z[T]
}


package inf1 {
  import scala.annotation.implicitNotFound

  @implicitNotFound(msg = "Cannot construct a collection of type ${To} with elements of type ${Elem} based on a collection of type ${From}.") // no warn
  trait CannotBuildFrom[-From, -Elem, +To]
}

package inf2 {
  @scala.annotation.implicitNotFound(msg = "Cannot construct a collection of type ${To} with elements of type ${Elem} based on a collection of type ${From}.") // no warn
  trait CannotBuildFrom[-From, -Elem, +To]
}

package inf3 {
  @scala.annotation.implicitNotFound("Cannot construct a collection of type ${To} with elements of type ${Elem} based on a collection of type ${From}.") // no warn
  trait CannotBuildFrom[-From, -Elem, +To]
}

package curry {
  class A {
    def bunko()(x: Int): Int = 5
    def groucho(): Int = 5
    def dingo()()()()()(): Int = 5 // kind of nuts this can be evaluated with just 'dingo', but okay
    def calico[T1, T2]()()(): Int = 5 // even nutsier
    def palomino[T1, T2]()(y: Int = 5)(): Int = 5 // even nutsier

    def f1 = "I was picked up by the $bunko squad" // no warn
    def f2 = "I salute $groucho" // warn 7
    def f3 = "I even salute $dingo" // warn 8
    def f4 = "I also salute $calico" // warn 9
    def f5 = "I draw the line at $palomino" // no warn
  }
}

package companions {
  class X
  object X
  class C {
    def f1 = "$X"   // nowarn companion
    def f2 = "$Byte"   // nowarn companion
    def f3 = "$Char"   // nowarn companion
    def f4 = "$Short"   // nowarn companion
    def f5 = "$Int"   // nowarn companion
    def f6 = "$Float"   // nowarn companion
    def f7 = "$Double"   // nowarn companion
    def f8 = "$Character"   // nowarn companion
    def f9 = "$Integer"   // nowarn companion
    def f0 = "$companions"   // nowarn companion
  }
}
package object companions

object `t10125 avoid forcing owners` {
  implicit class HasIr(s: String) {
    def ir: Int = 1234
  }

  val bar = "$bar".ir   // nowarn owner

  val x = "$x" // nowarn owner
}

object t10456 {
  @deprecated("${myProperty}")
  var myProperty: String = _
}
