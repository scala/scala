object Test {

  trait A extends AnyRef {
    def f : Int = 1
  }

  class B extends AnyRef with A {
    override def f : Int = super[A].f
  }

  def main(args: Array[String]) =
    Console.println(new B().f);
}



