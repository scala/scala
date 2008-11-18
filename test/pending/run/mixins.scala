trait A { val x: Int }
trait B extends A { override val x = 1 } // with A
trait C extends A { override val x = 2 } // with A
object Test extends B with C with Application {
  println(x)
}
