module test {

  trait A { type T; }

  trait B { type T; }

  def functor(x: A): B with { type T = x.T } =
    new B with {
      type T = x.T;
    };

  val a = new  A with { type T = String };
  val b = functor(a);

  val s: b.T = "coucou";

}
