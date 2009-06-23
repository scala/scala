object Test {
  class Op[I];
  class IntOp extends Op[Int];

  class Rich(x : Double) {
    def +       (op : IntOp) = op;
    def +    [I](op : Op[I]) = op;
    def plus [I](op : Op[I]) = op;
  }

  implicit def iToRich(x : Double) =
    new Rich(x);

  // fails to compile
  val failure = 1.0 + new Op[Int];

  // works as expected --
  //   problem isn't in adding new "+"
  val a = 1.0 + new IntOp;

  // works as expected --
  //   problem isn't in binding type variable I
  val b = 1.0 plus new Op[Int];

  // works as expected --
  //   problem isn't in using Rich.+[I](op : Op[I])
  val c = iToRich(1.0) + new Op[Int];
}
