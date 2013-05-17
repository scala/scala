trait A { def x : Int }
trait B { val m : A }
object C extends B {
  object m extends A { def x = 5 }
}
object Test {
  // The type annotation here is necessary, otherwise
  // the compiler would reference C$m$ directly.
  def o1 : B = C
  def o2 = C

  def main(argv : Array[String]) : Unit = {
    println(o1.m.x)
    println(o2.m.x)
  }
}
