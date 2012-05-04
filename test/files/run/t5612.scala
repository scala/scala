object L extends Enumeration {
  val One, Two, Three = Value
}

class Foo {
  def foo(xs: List[L.Value]) {
    import scala.util.control.Breaks.{break, breakable}
    println("START for " + xs)
    breakable {
      for (x <- xs) {
        x match {
          case L.One => println("ONE"); return
          case L.Two => println("TWO")
          case L.Three => println("THREE"); break
        }
      }
    }
    println("FINISH")
  }
}

object Test {
  def main(args: Array[String]) {
    val f = new Foo()
    val l = List(L.Two, L.Two, L.One, L.Three)
    f.foo(l)
  }
}
