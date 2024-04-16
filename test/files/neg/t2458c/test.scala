//> using options -Xsource:2.13
import q.X._

package p {
  object Test {
    def main(args: Array[String]): Unit = println {
      // both defined in p in other unit, and imported from q.X = ambiguous
      f() + g()
    }
  }
}
package q {
  object X { def f = "bye" ; def g = "hi" }
}
