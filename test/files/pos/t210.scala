trait Lang1 {
  trait Exp;
  trait Visitor { def f(left: Exp): Unit }
  class Eval1 extends Visitor { self: Visitor =>
    def f(left: Exp) = ()
  }
}

trait Lang2 extends Lang1 {
  class Eval2 extends Eval1 { self: Visitor => }
}
/*
object Main with App {
  val lang2 = new Lang2 {}
  val eval = new lang2.Eval2
}
*/
