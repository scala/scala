package expAbstractData

/** A base class consisting of
 *   - a root trait (i.e. abstract class) `Exp' with an `eval' function
 *   - an abstract type `exp' bounded by `Exp'
 *   - a concrete instance class `Num' of `Exp' for numeric literals
 */
trait Base {
  type exp <: Exp

  trait Exp {
    def eval: Int
  }
  class Num(v: Int) extends Exp { self: exp =>
    val value = v
    def eval = value
  }
}

object testBase extends App with Base {
  type exp = Exp
  val term = new Num(2);
  Console.println(term.eval)
}

/** Data extension: An extension of `Base' with `Plus' expressions
 */
trait BasePlus extends Base {
  class Plus(l: exp, r: exp) extends Exp { self: exp =>
    val left = l
    val right = r
    def eval = left.eval + right.eval
  }
}

/** Operation extension: An extension of `Base' with 'show' methods.
 */
trait Show extends Base {
  type exp <: Exp1

  trait Exp1 extends Exp {
    def show: String
  }
  class Num1(v: Int) extends Num(v) with Exp1 { self: exp with Num1 =>
    def show = value.toString()
  }
}

/** Operation extension: An extension of `BasePlus' with 'show' methods.
 */
trait ShowPlus extends BasePlus with Show {
  class Plus1(l: exp, r: exp) extends Plus(l, r) with Exp1 { self: exp with Plus1 =>
    def show = left.show + " + " + right.show
  }
}
