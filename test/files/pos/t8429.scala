trait Must {  def musta(str: String, i: Int): Unit }

object Mustare {
  def takesM(m: Must) = ???
  takesM{ (a, b) => println } // ok
  takesM{ case (a: String, b: Int) => println("") } // should also be accepted
}
