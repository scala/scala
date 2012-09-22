package bar {
  object bippy extends (Double => String) {
    def apply(x: Double): String = "Double"
  }
}

package object bar {
  def bippy(x: Int, y: Int, z: Int) = "(Int, Int, Int)"
}

object Test {
  def main(args: Array[String]): Unit = {
    println(bar.bippy(5.5d))
    println(bar.bippy(1, 2, 3))
  }
}

/****

% scalac3 a.scala
a.scala:13: error: not enough arguments for method bippy: (x: Int, y: Int, z: Int)String.
Unspecified value parameters y, z.
    println(bar.bippy(5.5d))
                     ^
one error found

# Comment out the call to bar.bippy(5.5d) - compiles
% scalac3 a.scala

# Compiles only from pure source though - if classes are present, fails.
% scalac3 a.scala
a.scala:2: error: bippy is already defined as method bippy in package object bar
  object bippy extends (Double => String) {
         ^
one error found

****/
