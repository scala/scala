// a.scala
// Fri Jan 13 11:31:47 PST 2012

package foo {
  package object bar {
    def duh(n: Long)   = println("long")
    def duh(n: Double) = println("double")

    def duh2(n: Double) = println("double")
    def duh2(n: Long)   = println("long")
  }
  package bar {
    object Main {
      def main(args:Array[String]) {
        duh(33L)
        bip.bar.duh(33L)
        duh(33d)
        bip.bar.duh(33d)

        duh2(33L)
        bip.bar.duh2(33L)
        duh2(33d)
        bip.bar.duh2(33d)
      }
    }
  }
}

package bip {
  trait Duh {
    def duh(n: Long)   = println("long")
    def duh(n: Double) = println("double")
  }
  trait Duh2 {
    def duh2(n: Double) = println("double")
    def duh2(n: Long)   = println("long")
  }

  package object bar extends Duh with Duh2 { }
  package bar {
    object Main {
      def main(args:Array[String]) {
        duh(33L)
        bip.bar.duh(33L)
        duh(33d)
        bip.bar.duh(33d)

        duh2(33L)
        bip.bar.duh2(33L)
        duh2(33d)
        bip.bar.duh2(33d)
      }
    }
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    foo.bar.Main.main(null)
    bip.bar.Main.main(null)
  }
}
