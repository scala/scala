object O {
  case class N()
  object P
}

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
    Outer.Inner.i // we still don't initialize Outer here (but should we?)

    {println("About to reference Inner.i"); Outer}.Inner.i // Outer will be initialized.

    {println("About to reference O.N"        ); O}.N

    {println("About to reference O.N"        ); O}.N

    {println("About to reference O.N.apply()"); O}.N.apply()
  }
}

