trait Lang1 {
  trait Exp;
  trait Visitor { def f(left: Exp): unit; }
  class Eval1 requires Visitor extends Visitor {
    def f(left: Exp) = ();
  }
}

trait Lang2 extends Lang1 {
  class Eval2 requires Visitor extends Eval1;
}
/*
object Main with Application {
  val lang2 = new Lang2 {};
  val eval = new lang2.Eval2;
}
*/
