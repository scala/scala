object Test
{
    def main(args : Array[String]) : Unit = Console.println(new C(1))
}

abstract class A(val x : Int)

class C(x : Int) extends A(x)
{
  override def toString() = "OK"
  val v = new D
  class D { def value = x }
}
