object test {

  trait A { type T; }

  trait B { type T; }

  def functor(x: A): B { type T = x.T } =
    new B {
      type T = x.T;
    };

  val a = new  A { type T = String };
  val b = functor(a);

  val s: b.T = "coucou";

}
