module test {

  trait A { type T; }

  trait B { type T; }

  /** def functor(x: A): B with { type T = x.T } */
  abstract class functor() {
    val arg: A;
    val res: B with { type T = arg.T } =
      new B with { type T = arg.T; };
  }

  val a = new  A with { type T = String };
  /** val b: B with { type T = String } = functor(a) */
  val b: B with { type T = String } = {
    val tmp = new functor() with { val arg = a };
    tmp.res
  }

}
