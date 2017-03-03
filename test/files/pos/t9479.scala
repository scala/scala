trait Predefs {
  def bridge(p: String): Unit = ???
  def bridge(p: Any): Unit = ???
}

package object molecule extends Predefs

package molecule {
  package process {
    class Test {
      def main(): Unit = bridge(null, null)
    }
  }
}

