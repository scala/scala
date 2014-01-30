trait A { private val x = 1 }
trait B { val x = 2 }
trait C1 extends B with A { println(x) }
trait C2 extends A with B { println(x) }

object Test {
  def main(args: Array[String]): Unit = {
    new C1 { }
    new C2 { }
  }
}
