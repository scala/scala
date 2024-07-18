//> using options -Werror -Xlint
//

import annotation.nowarn

trait Pattern {

  trait NumericOps[T] extends Serializable {

    def zero: T

    def add(a: T, b: T): T
    def add(a: T, b: T, c: T): T = add(a, add(b, c))

    def sum(terms: Iterable[T]) = terms.foldLeft(zero)(add)
    def sum(terms: Iterator[T]) = terms.foldLeft(zero)(add)
  }

  trait Expr[T] {

    /** Returns arguments of this operator */
    def args: Iterable[Expr[_]]

    def + (other: Expr[T])(implicit n: NumericOps[T]) = Add(List(this, other))

    def specialize(implicit num: NumericOps[T]): Expr[T] =
      this match {
        case Add(Seq(a, b))    => Add2(a, b)
        case Add(Seq(a, b, c)) => Add3(a, b, c)
        case x                 => x
      }
  }

  trait TwoArg[T] extends Expr[T]  {
    val left: Expr[T]
    val right: Expr[T]
    val args = List(left, right)
  }

  trait ManyArg[T] extends Expr[T]

  case class Add[T](args: Iterable[Expr[T]])(implicit @nowarn num: NumericOps[T]) extends ManyArg[T] {
    override def toString = "(" + args.mkString(" + ") + ")"
  }

  case class Add2[T](left: Expr[T], right: Expr[T])(implicit @nowarn num: NumericOps[T]) extends TwoArg[T] {
    override def toString = "(" + left + " + " + right + ")"
  }
  case class Add3[T](a1: Expr[T], a2: Expr[T], a3: Expr[T])(implicit @nowarn num: NumericOps[T]) extends ManyArg[T] {
    val args = List(a1, a2, a3)
    override def toString = "(" + a1 + " + " + a2 + " + " +  a3 + ")"
  }
}
