// a.scala
// Fri Jan 13 11:31:47 PST 2012

package foo {
  object regular extends Duh {
    def buh(n: Long)   = println("long")
    def buh(n: Double) = println("double")
  }
  class regular {
    import regular._

    duh(33L)
    duh(3.0d)
    foo.regular.duh(33L)
    foo.regular.duh(3.0d)
    buh(66L)
    buh(6.0d)
    foo.regular.buh(66L)
    foo.regular.buh(6.0d)
  }

  trait Duh {
    def duh(n: Long)   = println("long")
    def duh(n: Double) = println("double")
  }
  package object bar extends Duh {
    def buh(n: Long)   = println("long")
    def buh(n: Double) = println("double")
  }
  package bar {
    object Main {
      def main(args:Array[String]) {
        duh(33L)
        duh(3.0d)
        foo.bar.duh(33L)
        foo.bar.duh(3.0d)
        buh(66L)
        buh(6.0d)
        foo.bar.buh(66L)
        foo.bar.buh(6.0d)
      }
    }
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    foo.bar.Main.main(null)
  }
}
