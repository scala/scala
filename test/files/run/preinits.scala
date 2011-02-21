trait A { val x: Int; println("A") }
trait B extends { override val x = 1 } with A { println("B") }
trait C extends { override val x = 2 } with A
object Test extends B with C with App {
  println(x)
}
