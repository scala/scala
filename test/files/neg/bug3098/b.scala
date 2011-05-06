// Test.scala
object Test {
  def f = (null: T) match {
    case _: A => println("A")
    case _: B => println("B")
    // no C
  }
}
