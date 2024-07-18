//> using options -nowarn
import util.chaining._

object Test {
  def main(args: Array[String]) = 42.tap(res => assert(res == 42))
}

// test that partest is using scala runner to execute this test.
// With warnings enabled:
/*
t7448.scala:7: warning: not a valid main method for Test,
  because main methods must have the exact signature `(Array[String]): Unit`, though Scala runners will forgive a non-Unit result.
  To define an entry point, please define the main method as:
    def main(args: Array[String]): Unit

  def main(args: Array[String]) = 42.tap(res => assert(res == 42))
      ^
 */
