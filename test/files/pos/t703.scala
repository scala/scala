object Go {
  trait A {
    def f : Unit; // = Console.println("A");
  }
  trait B extends A {
    abstract override def f = {
      super.f;
      Console.println("B");
    }
  }
  trait C extends A {
    abstract override def f = {
      super.f;
      Console.println("C");
    }
  }
  trait D extends B with C {
    abstract override def f = {
      super.f;
    }
  }
  class Super extends A {
    def f: Unit = Console.println("A")
  }
  def main(args : Array[String]) : Unit = {
    object d extends Super with D
    d.f;
  }
}
