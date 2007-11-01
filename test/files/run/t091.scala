trait A { def x : Int }
trait B { val m : A }
object C extends B {
  object m extends A { def x = 5 }
}
object Test {
    // The type annotation here is necessary, otherwise
    // the compiler would reference C$m$ directly.
    def o : B = C
    def main(argv : Array[String]) : Unit = {
        println(o.m.x)
    }
}
