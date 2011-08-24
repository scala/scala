trait B {
  private val foo: String = "B"
  def f = println(foo)
}
trait A { val foo: String = "A" }
object Test extends App with B with A {
  println(foo) // prints "A", as expected
  f            // prints "B", as expected
}
