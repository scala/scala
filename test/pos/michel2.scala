object Test {

  trait A extends Object {
    def f : Int = 1
  }

  class B extends Object with A {
    override def f : Int = super[A].f
  }

  def main(args: Array[String]) =
    System.out.println(new B().f);
}



