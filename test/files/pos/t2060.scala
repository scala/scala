/* The problem here is that we cannot insert an implicit to
 * add a polymorphic method which is then instantiated to the
 * right type. When searching for the implicit in the `fails to compile
 * line':
 *
 *   val failure = 1.0 + new Op[Int]
 *
 * we reduce the problem to finding a function from Double to
 * {+: _ >: Op[Int] <: Any}, that is, a method which takes
 * an argument which is an Op[Int] or a supertype thereof.
 * Class Rich is not a subtype of this structural record, because
 * polymorphic method instantiation is not contained in subtyping.
 * That is: The method type [I](op : Op[I]): Op[I] is not a subtype
 * of (Op[Int]): Op[Int].
 * At present it is unclear whether this problem can be solved.
 */
object Test {
  class Op[I];
  class IntOp extends Op[Int];

  class Rich(x : Double) {
    def +       (op : IntOp): IntOp = op;
    def +    [I](op : Op[I]): Op[I] = op;
    def plus [I](op : Op[I]): Op[I] = op;
  }

  implicit def iToRich(x : Double) =
    new Rich(x);

  // fails to compile
  val x = 1.0 + new Op[Int]

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
