object Outer {
  println("Outer")
  object Inner {
    println("Inner")
    def i {
      println("Inner.i")
    }
  }
}

object Test {
  def main(args: Array[String]) {
    Outer.Inner.i // we still don't initiialize Outer here (but should we?)

    // Outer still not initialized under Scala 2.10.0-
    // or with the backwards compatilibity option -Ydead-code-module-load
    //
    // Contrast with run/t4859.scala.
    {println("About to reference Inner.i"); Outer}.Inner.i
  }
}
