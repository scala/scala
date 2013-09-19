class A {
  val bippy = 123

  def f = "Put the $bippy in the $bippy!" // warn
}

class B {
  val dingus = 123

  def f = "Put the $bippy in the $bippy!" // no warn
}

class C {
  def f = """Put the ${println("bippy")} in the bippy!""" // warn
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
      def f = "$beppo was a marx bros who saw dollars."  // warn
    }
  }
  class E {
    def f = "$aleppo is a pepper and a city."  // warn
  }
  class Bar {
    private def bar = 8
  }
  class Baz extends Bar {
    def f = "$bar is private, shall we warn just in case?" // warn
  }
  class G {
    def g = "$greppo takes an arg"  // no warn
    def z = "$zappos takes an arg too"  // no warn
    def h = "$hippo takes an implicit"  // warn
  }
}
