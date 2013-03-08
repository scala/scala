object Test {
  
  def testit(f: Int => Int, x: Int) = f(x)

  // test from constructor
  val r1 = testit(x => x + 1, 0)
  println(s"In constructor should be 1: $r1")

  // test from method
  def main(args: Array[String]) {
    val r2 = testit(x => x + 1, 1)
    println(s"In method should be 2: $r2")

    // test in super call
    val r3 = (new Bar(2)).result
    println(s"In super constructor should be 3: $r3")
  }

}

class Foo(f: Int => Int, x: Int) {
  def result = f(x)
}

class Bar(x: Int) extends Foo(x => x + 1, x)