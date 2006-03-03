abstract class C {

  type T <: Any;

}

abstract class D[S <: C](_c: S) extends C {

  val c: S = _c;
  type T <: c.T;

}

abstract class E(e: E) extends D[E](e);

object Test {

  def f(e: E): Unit = {
    def g(t: e.T): Unit = {
      val i: Int = t;
      ()
    }
    ()
  }

}
