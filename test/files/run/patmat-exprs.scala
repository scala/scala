import runtime.ScalaRunTime

object Test {
  val p = new Pattern { }
  import p._
  implicit object IntOps extends NumericOps[Int] {
    def zero = 0
    def one = 1

    def add(a: Int, b: Int): Int = a + b
    def sub(a: Int, b: Int): Int = a - b
    def mul(a: Int, b: Int): Int = a * b
    def mul(a: Int, b: Double): Int = (a * b).toInt
    def div(a: Int, b: Int): Int = a / b
    def div(a: Int, b: Double): Int = (a / b).toInt
    def similar(a: Int, b: Int): Boolean = a == b
    def abs(a: Int): Double = math.abs(a).toDouble
    def sqr(a: Int): Int = a * a
    def sqrt(a: Int): Int = math.sqrt(a).toInt
    def log(a: Int): Int = math.log(a).toInt
    def exp(a: Int): Int = math.exp(a).toInt
    def sin(a: Int): Int = math.sin(a).toInt
    def cos(a: Int): Int = math.cos(a).toInt

    def fromDouble(a: Double): Int = a.toInt
    def fromInt(a: Int): Int = a
  }

  def main(args: Array[String]): Unit = {
    println((5: Expr[Int]) + 10 + 15 * 20)
  }
}


trait Pattern {
  // For trying out 2.7.7
  //
  // type Numeric[T]
  // import java.io.Serializable
  // 
  // implicit def compat27a[T](x: Iterable[T]) = new {
  //   def iterator: Iterator[T] = x.elements
  //   def sum: Int = 5
  //   def collect[U](pf: PartialFunction[T, U]): Iterable[U] = x map pf
  // }

  /** Function that returns object of the same type it was passed */
  trait EndoFunction[-A] {
    def apply[B <: A](x: B): B
  }

  /** Allows for smart construction of EndoFunction from an ordinary function */
  object EndoFunction {
    def apply[A](f: A => A): EndoFunction[A] = new EndoFunction[A] {
      def apply[B <: A](x: B): B = f(x).asInstanceOf[B]
    }
  }

  trait NumericOps[T] extends Serializable {
    def zero: T
    def one: T
    def two = add(one, one)
    def three = add(two, one)

    def add(a: T, b: T): T
    def add(a: T, b: T, c: T): T = add(a, add(b, c))
    def sub(a: T, b: T): T
    def mul(a: T, b: T): T
    def mul(a: T, b: Double): T
    def div(a: T, b: T): T
    def div(a: T, b: Double): T
    def similar(a: T, b: T): Boolean
    def neg(a: T) = sub(zero, a)
    def abs(a: T): Double
    def sqr(a: T): T
    def sqrt(a: T): T
    def log(a: T): T
    def exp(a: T): T
    def sin(a: T): T
    def cos(a: T): T
    def tan(a: T): T = div(sin(a), cos(a))

    def fromDouble(a: Double): T
    def fromInt(a: Int): T

    def sum(terms: Iterable[T]) = terms.foldLeft(zero)(add)
    def sum(terms: Iterator[T]) = terms.foldLeft(zero)(add)
    def product(terms: Iterable[T]) = terms.foldLeft(one)(mul)
    def product(terms: Iterator[T]) = terms.foldLeft(one)(mul)


    def similar(a: Iterable[T], b: Iterable[T]): Boolean = {
      val i1 = a.iterator
      val i2 = b.iterator
      while (i1.hasNext && i2.hasNext)
        if (!similar(i1.next, i2.next))
          return false;
      true;
    }
  }

  /**
   * Simple expression interpreter with some basic symbolic manipulation.
   * Able to evaluate derivatives.
   */

  trait Expr[T] {

    import Expr._

    /** Evaluates value of the expression. */
    def eval(context: Any => Any): T

    /** Symbolically calculates derivative of this expression. Does not simplify it. */
    def derivative(variable: Var[T]): Expr[T]

    /** Returns arguments of this operator */
    def args: Iterable[Expr[_]]

    /** Transforms arguments of this operator by applying given function. */
    def mapArgs(f: EndoFunction[Expr[_]]): Expr[T]

    /** Transforms this operator and its arguments by applying given function */
    def map(f: EndoFunction[Expr[_]]): Expr[T] =
      f(mapArgs(EndoFunction[Expr[_]](x => x.map(f))))

    /** Folds all subexpressions in this expression in depth-first order */
    def fold[A](v: A)(f: (A, Expr[_]) => A): A =
      f(args.foldLeft(v) { (a, b) => b.fold(a)(f) }, this)

    /** Replaces all occurrences of one subexpression with another one */
    def replace(from: Expr[_], to: Expr[_]): Expr[T] =
      map(EndoFunction[Expr[_]](x => if (x == from) to else x))

    /** Returns true if this expression contains given subexpression */
    def contains(s: Expr[_]): Boolean =
      this == s || args.exists(_ contains s)

    /** Counts number of occurrences of the given subexpression. */
    def count(condition: Expr[_] => Boolean): Int =
      (if (condition(this)) 1 else 0) + args.map(_.count(condition)).sum

    /** Executes some code for every subexpression in the depth-first order */
    def foreach[U](block: Expr[_] => U): Unit = {
      args.foreach(_.foreach(block))
      block(this)
    }

    /** Collects subexpressions successfully transformed by the given partial function, in depth-first order. */
    def collect[U](f: PartialFunction[Expr[_], U]): List[U] = {
      val a = args.flatMap(_.collect(f)).toList
      if (f.isDefinedAt(this)) (f(this) :: a) else a
    }

    def leaves: List[Leaf[T]] = collect { case l: Leaf[T] => l }

    def + (other: Expr[T])(implicit n: NumericOps[T]) = Add(List(this, other))
    def - (other: Expr[T])(implicit n: NumericOps[T]) = Sub(this, other)
    def * (other: Expr[T])(implicit n: NumericOps[T]) = Mul(this, other)
    def / (other: Expr[T])(implicit n: NumericOps[T]) = Div(this, other)

    def unary_- (implicit n: NumericOps[T]) = Neg(this)
    def sqr(implicit n: NumericOps[T]) = Sqr(this)

    def < (other: Expr[T])(implicit n: NumericOps[T], o: Ordering[T]) = LT(this, other)
    def <= (other: Expr[T])(implicit n: NumericOps[T], o: Ordering[T]) = LE(this, other)
    def > (other: Expr[T])(implicit n: NumericOps[T], o: Ordering[T]) = GT(this, other)
    def >= (other: Expr[T])(implicit n: NumericOps[T], o: Ordering[T]) = GE(this, other)

    private def generalize(implicit num: NumericOps[T]): Expr[T] = {
      this match {
        case Add2(a, b) => Add(a :: b :: Nil)
        case Add3(a, b, c) => Add(a :: b :: c :: Nil)
        case Sub(a, b) => Add(a :: Neg(b) :: Nil)
        case Add(x) => Add(x flatMap {
          case Neg(Add(y)) => y.map(Neg(_))
          case Add(y)      => y
          case y           => y :: Nil
        })
        case x => x
      }
    }

    private def specialize(implicit num: NumericOps[T]): Expr[T] = {
      this match {
        case Add(Seq(a, b)) => Add2(a, b)
        case Add(Seq(a, b, c)) => Add3(a, b, c)
        case x => x
      }
    }

    /** Eliminates common negated components of a sum */
    private def reduceComponents(components: List[Expr[T]])(implicit num: NumericOps[T]): List[Expr[T]] = {
      val pairs =
        for (a <- components; b <- components if Neg(a) == b || a == Neg(b))
          yield (a, b)
      pairs.foldLeft(components) { (c, pair) =>
        if (c.contains(pair._1) && c.contains(pair._2))
          c.diff(pair._1 :: pair._2 :: Nil)
        else
          c
      }
    }


    /** Simplifies this expression to make evaluation faster and more accurate.
     *  Performs only one pass. */
    private def reduce(implicit num: NumericOps[T]): Expr[T] = {
      this match {
        case Add(Seq(Neg(x), Neg(y), Neg(z))) => Neg(Add(List(x, y, z)))
        case Add(Seq(Mul(x, y), z)) if (x == z) => Mul(x, Add(List(y, One[T])))
        case Add(Seq(Mul(x, y), z)) if (y == z) => Mul(y, Add(List(z, One[T])))
        case Add(Seq(Mul(x, y), Mul(u, w))) if (x == u) => Mul(x, Add(List(y, w)))
        case Add(Seq(Mul(x, y), Mul(u, w))) if (y == w) => Mul(y, Add(List(x, u)))
        case Add(Seq(Add(x), Add(y))) => Add(x.toList ::: y.toList).simplify
        case Add(Seq(Add(x), y)) => Add(y :: x.toList).simplify
        case Add(Seq(x, Add(y))) => Add(x :: y.toList).simplify
        case Add(x) => {
          val noZeros = x.filter(_ != Zero[T])
          val noOnes = noZeros.map { case y: One[_] => Const(num.one); case y => y }
          val constant = num.sum(noOnes.collect { case c: Const[T] => c.value })
          val rest = noOnes.filter(x => !x.isInstanceOf[Const[_]]).toList
          val reduced = reduceComponents(rest)
          val args = if (num.similar(constant, num.zero)) reduced else reduced ::: Const(constant) :: Nil
          args.size match {
            case 0 => Zero[T]
            case 1 => args.head
            case 2 => Add2(args(0), args(1))
            case 3 => Add3(args(0), args(1), args(2))
            case _ => Add(args)
          }
        }
        case Sub(x: Zero[_], y) => Neg(y)
        case Sub(x, y: Zero[_]) => x
        case Sub(x, y) if x == y => Zero[T]
        case Sub(Mul(x, y), z) if (x == z) => Mul(x, Sub(y, One[T]))
        case Sub(Mul(x, y), z) if (y == z) => Mul(y, Sub(z, One[T]))
        case Sub(Mul(x, y), Mul(u, w)) if (x == u) => Mul(x, Sub(y, w))
        case Sub(Mul(x, y), Mul(u, w)) if (y == w) => Mul(y, Sub(x, u))
        case Mul(x: Zero[_], y) => Zero[T]
        case Mul(x, y: Zero[_]) => Zero[T]
        case Mul(x: One[_], y) => y
        case Mul(x, y: One[_]) => x
        case Mul(Neg(x: One[_]), y) => Neg(y)
        case Mul(x, Neg(y: One[_])) => Neg(x)

        case Mul(x, y) if (x == y) => Sqr(x)
        case Div(x: Zero[_], y) => Zero[T]   // warning: possibly extends domain
        case Div(x, y: One[_]) => x
        case Div(Sqr(x), y) if x == y => x
        case Div(Mul(x, y), z) if (x == z) => y
        case Div(Mul(x, y), z) if (y == z) => y
        case Div(Mul(Mul(x, y), z), w) if (x == w) => Mul(y, z)
        case Div(Mul(Mul(x, y), z), w) if (y == w) => Mul(x, z)
        case Div(Mul(z, Mul(x, y)), w) if (x == w) => Mul(y, z)
        case Div(Mul(z, Mul(x, y)), w) if (y == w) => Mul(x, z)
        case Div(Mul(x, y), Mul(u, w)) if (x == u) => Div(y, w)
        case Div(Mul(x, y), Mul(u, w)) if (y == w) => Div(x, u)
        case Div(x: One[_], y) => Inv(y)
        case Div(x, Sqr(y)) if x == y => Inv(y)
        case Div(Mul(x, y), Sqr(Mul(u, w))) if x == u && y == w => Inv(Mul(x, y))
        case Div(x, y) if x == y => One[T]

        case Mul(Neg(a), Neg(b)) => Mul(a, b)
        case Div(Neg(a), Neg(b)) => Div(a, b)

        case Neg(x: Zero[_]) => Zero[T]
        case Neg(x: One[_]) => Const(num.neg(num.one))
        case Sub(Const(x), Const(y)) => const(num.sub(x, y))
        case Mul(Const(x), Const(y)) => const(num.mul(x, y))
        case Div(Const(x), Const(y)) => const(num.div(x, y))
        case Neg(Const(x)) => const(num.neg(x))
        case Sqr(Const(x)) => const(num.sqr(x))

        case Mul(Const(x), Mul(Const(y), z)) => Mul(const(num.mul(x, y)), z)
        case Mul(Const(x), Mul(y, Const(z))) => Mul(const(num.mul(x, z)), y)
        case Mul(Mul(Const(y), z), Const(x)) => Mul(const(num.mul(x, y)), z)
        case Mul(Mul(y, Const(z)), Const(x)) => Mul(const(num.mul(x, z)), y)

        case Const(x) if x == num.one => One[T]
        case Const(x) if x == num.zero => Zero[T]      
      
        case Sub(x, Neg(y)) => Add(List(x, y))
        case Sub(Neg(x), y) => Neg(Add(List(x, y)))
        case Neg(Neg(x)) => x
        case Neg(Mul(a: Const[T], x)) => Mul(const(num.neg(a.value)), x)
        case Neg(Mul(x, a: Const[T])) => Mul(const(num.neg(a.value)), x)
        case Neg(Div(Neg(a), b)) => Div(a, b)
        case Neg(Div(a, Neg(b))) => Div(a, b)
        case Neg(Mul(Neg(a), b)) => Mul(a, b)
        case Neg(Mul(a, Neg(b))) => Mul(a, b)

        case Log(Exp(x)) => x
        case x => x
      }
    }

    private def optimizeWith(f: Expr[T] => Expr[T]): Expr[T] = {
      f(mapArgs(EndoFunction[Expr[_]](
        a => a match { case x: Expr[T] => x.optimizeWith(f) }
      )))
    }

    /** Simplifies this expression to make evaluation faster and more accurate.*/
    def simplify(implicit num: NumericOps[T]): Expr[T] = {
      val a1 = optimizeWith(_.generalize)
      val a2 = a1.optimizeWith(_.generalize)
      val b = a2.optimizeWith(_.reduce)
      val c = b.optimizeWith(_.reduce)
      val d = c.optimizeWith(_.specialize)
      d
    }
  }


  trait Leaf[T] extends Expr[T] {
    val args = List[Expr[T]]()
    def mapArgs(f: EndoFunction[Expr[_]]) = this
  }

  trait OneArg[T] extends Expr[T] {
    val expr: Expr[T]
    val args = List(expr)
  }


  trait TwoArg[T] extends Expr[T]  {
    val left: Expr[T]
    val right: Expr[T]
    val args = List(left, right)
  }

  trait ManyArg[T] extends Expr[T]

  /** Marker trait for specifying that you can safely divide by this */
  trait NonZero[T] extends Expr[T]

  case class Const[T](value: T)(implicit num: NumericOps[T]) extends Leaf[T] with NonZero[T] {
    def derivative(variable: Var[T]) = Zero[T]
    def eval(f: Any => Any) = value
    override def toString = value.toString
  }


  case class Zero[T] (implicit num: NumericOps[T]) extends Leaf[T] {
    def derivative(variable: Var[T]) = Zero[T]
    def eval(f: Any => Any) = num.zero
    override def toString = "0"
  }

  case class One[T] (implicit num: NumericOps[T]) extends Leaf[T] {
    def derivative(variable: Var[T]) = Zero[T]
    def eval(f: Any => Any) = num.one
    override def toString = "1"
  }

  abstract class Var[T](implicit num: NumericOps[T]) extends Leaf[T] {
    def derivative(variable: Var[T]) = if (variable == this) One[T] else Zero[T]
    def eval(f: Any => Any) = f(this).asInstanceOf[T]
  }

  case class NamedVar[T](name: String)(implicit num: NumericOps[T]) extends Var[T] {
    override lazy val hashCode = ScalaRunTime._hashCode(this)
    override def toString = name
  }

  case class Add[T](args: Iterable[Expr[T]])(implicit num: NumericOps[T]) extends ManyArg[T] {
    def eval(f: Any => Any) = num.sum(for (i <- args.iterator) yield i.eval(f))
    def derivative(v: Var[T]) = Add(args.map(_.derivative(v)))
    def mapArgs(f: EndoFunction[Expr[_]]) = Add(args map (x => f(x)))
    override def toString = "(" + args.mkString(" + ") + ")"
    override lazy val hashCode = ScalaRunTime._hashCode(this);
  }


  case class Add2[T](left: Expr[T], right: Expr[T])
                    (implicit num: NumericOps[T]) extends TwoArg[T] {
    def eval(f: Any => Any) = num.add(left.eval(f), right.eval(f))
    def derivative(v: Var[T]) = Add2(left.derivative(v), right.derivative(v))
    def mapArgs(f: EndoFunction[Expr[_]]) = Add2(f(left), f(right))
    override def toString = "(" + left + " + " + right + ")"
    override lazy val hashCode = ScalaRunTime._hashCode(this);
  }

  case class Add3[T](a1: Expr[T], a2: Expr[T], a3: Expr[T])
                                  (implicit num: NumericOps[T]) extends ManyArg[T] {
    val args = List(a1, a2, a3)
    def eval(f: Any => Any) = num.add(a1.eval(f), a2.eval(f), a3.eval(f))
    def derivative(v: Var[T]) = Add3(a1.derivative(v), a2.derivative(v), a3.derivative(v))
    def mapArgs(f: EndoFunction[Expr[_]]) = Add3(f(a1), f(a2), f(a3))
    override def toString = "(" + a1 + " + " + a2 + " + " +  a3 + ")"
    override lazy val hashCode = ScalaRunTime._hashCode(this);
  }


  case class Sub[T](left: Expr[T], right: Expr[T])
                                 (implicit num: NumericOps[T]) extends TwoArg[T] {
    def derivative(v: Var[T]) = Sub(left.derivative(v), right.derivative(v))
    def eval(f: Any => Any) = num.sub(left.eval(f), right.eval(f))
    def mapArgs(f: EndoFunction[Expr[_]]) = Sub(f(left), f(right))
    override def toString = "(" + left + " - " + right + ")"
    override lazy val hashCode = ScalaRunTime._hashCode(this);
  }

  case class Neg[T](expr: Expr[T])
                                 (implicit num: NumericOps[T]) extends OneArg[T] {
    def derivative(v: Var[T]) = Neg(expr.derivative(v))
    def eval(f: Any => Any) = num.neg(expr.eval(f))
    def mapArgs(f: EndoFunction[Expr[_]]) = Neg(f(expr))
    override def toString = "(-" + expr + ")"
    override lazy val hashCode = ScalaRunTime._hashCode(this);

  }

  case class Mul[T](left: Expr[T], right: Expr[T])
                                 (implicit num: NumericOps[T]) extends TwoArg[T] {
    def derivative(v: Var[T]) =
      Add(List(
        Mul(left, right.derivative(v)),
        Mul(right, left.derivative(v))))

    def eval(f: Any => Any) = num.mul(left.eval(f), right.eval(f))
    def mapArgs(f: EndoFunction[Expr[_]]) = Mul(f(left), f(right))
    override def toString = "(" + left + " * " + right + ")"
    override lazy val hashCode = ScalaRunTime._hashCode(this);
  }

  case class Div[T](left: Expr[T], right: Expr[T])
                                 (implicit num: NumericOps[T]) extends TwoArg[T] {

    // [f(x) / g(x)]' = [f(x) * 1 / g(x)]' = f'(x) * 1 / g(x) + f(x) * [1 / g(x)]' =
    //    f'(x) / g(x) + f(x) * [-1 / g(x) ^ 2] * g'(x) = (f'(x) * g(x) - f(x) * g'(x)) / g(x)^2
    def derivative(v: Var[T]) =
      Div(
        Sub(
          Mul(left.derivative(v), right),
          Mul(left, right.derivative(v))),
        Sqr(right)
      )

    def eval(f: Any => Any) = num.div(left.eval(f), right.eval(f))
    def mapArgs(f: EndoFunction[Expr[_]]) =
      Div(f(left), f(right))
    override def toString = "(" + left + " / " + right + ")"
    override lazy val hashCode = ScalaRunTime._hashCode(this);
  }

  case class Inv[T](expr: Expr[T])(implicit num: NumericOps[T]) extends OneArg[T] {

    // [1 / f(x)]' = - f'(x) / f(x) ^ 2
    def derivative(v: Var[T]) = Neg(Div(expr.derivative(v), Sqr(expr)))
    def eval(f: Any => Any) = num.div(num.one, expr.eval(f))
    def mapArgs(f: EndoFunction[Expr[_]]) = Inv(f(expr))
    override def toString = "(1 / " + expr + ")"
    override lazy val hashCode = ScalaRunTime._hashCode(this);
  }

  case class Sqr[T](expr: Expr[T])(implicit num: NumericOps[T]) extends OneArg[T] {
    // [f(x) ^ 2]' = 2 * f(x) * f'(x)
    def derivative(v: Var[T]) = Mul(Mul(Const(num.two), expr), expr.derivative(v))
    def eval(f: Any => Any) = num.sqr(expr.eval(f))
    def mapArgs(f: EndoFunction[Expr[_]]) = Sqr(f(expr))
    override def toString = expr + " ^ 2"
    override lazy val hashCode = ScalaRunTime._hashCode(this);
  }


  case class Log[T](expr: Expr[T])(implicit num: NumericOps[T]) extends OneArg[T] {
    def derivative(v: Var[T]) = Div(expr.derivative(v), expr)
    def eval(f: Any => Any) = num.log(expr.eval(f))
    def mapArgs(f: EndoFunction[Expr[_]]) = Log(f(expr))
    override def toString = "log(" + expr + ")"
    override lazy val hashCode = ScalaRunTime._hashCode(this);
  }


  case class Exp[T](expr: Expr[T])(implicit num: NumericOps[T]) extends OneArg[T] {
    def derivative(v: Var[T]) = Mul(expr.derivative(v), Exp(expr))
    def eval(f: Any => Any) = num.exp(expr.eval(f))
    def mapArgs(f: EndoFunction[Expr[_]]) = Exp(f(expr))
    override def toString = "exp(" + expr + ")"
    override lazy val hashCode = ScalaRunTime._hashCode(this);
  }

  case class Sqrt[T](expr: Expr[T])(implicit num: NumericOps[T]) extends OneArg[T] {
    def derivative(v: Var[T]) = Neg(Div(expr.derivative(v), Sqrt(expr)))
    def eval(f: Any => Any) = num.sqrt(expr.eval(f))
    def mapArgs(f: EndoFunction[Expr[_]]) = Sqrt(f(expr))
    override def toString = "sqrt(" + expr + ")"
    override lazy val hashCode = ScalaRunTime._hashCode(this);
  }


  case class Sin[T](expr: Expr[T])(implicit num: NumericOps[T]) extends OneArg[T] {
    def derivative(v: Var[T]) = Mul(expr.derivative(v), Cos(expr))
    def eval(f: Any => Any) = num.sin(expr.eval(f))
    def mapArgs(f: EndoFunction[Expr[_]]) = Sin(f(expr))
    override def toString = "sin(" + expr + ")"
    override lazy val hashCode = ScalaRunTime._hashCode(this);
  }


  case class Cos[T](expr: Expr[T])(implicit num: NumericOps[T]) extends OneArg[T] {
    def derivative(v: Var[T]) = Neg(Mul(expr.derivative(v), Sin(expr)))
    def eval(f: Any => Any) = num.cos(expr.eval(f))
    def mapArgs(f: EndoFunction[Expr[_]]) = Cos(f(expr))
    override def toString = "cos(" + expr + ")"
    override lazy val hashCode = ScalaRunTime._hashCode(this);
  }


  abstract class Compare[T](left: Expr[T], right: Expr[T], cmp: (T, T) => Boolean)(implicit num: NumericOps[T])
    extends Expr[Boolean] {
    def derivative(v: Var[Boolean]) = throw new IllegalStateException("Derivative of Boolean not allowed")
    def eval(f: Any => Any) = cmp(left.eval(f), right.eval(f))
    val args = List(left, right)
  }

  case class LE[T](left: Expr[T], right: Expr[T])(implicit num: NumericOps[T], ord: Ordering[T])
    extends Compare[T](left, right, ord.compare(_, _) <= 0) {
    def mapArgs(f: EndoFunction[Expr[_]]) = LE(
      f(left), f(right))
    override def toString = left.toString + " <= " + right.toString
  }

  case class LT[T](left: Expr[T], right: Expr[T])(implicit num: NumericOps[T], ord: Ordering[T])
    extends Compare[T](left, right, ord.compare(_, _) < 0) {
    def mapArgs(f: EndoFunction[Expr[_]]) = LT(
      f(left), f(right))
    override def toString = left.toString + " < " + right.toString
  }

  case class GE[T](left: Expr[T], right: Expr[T])(implicit num: NumericOps[T], ord: Ordering[T])
    extends Compare[T](left, right, ord.compare(_, _) >= 0) {
    def mapArgs(f: EndoFunction[Expr[_]]) = GE(
      f(left), f(right))
    override def toString = left.toString + " >= " + right.toString
  }

  case class GT[T](left: Expr[T], right: Expr[T])(implicit num: NumericOps[T], ord: Ordering[T])
    extends Compare[T](left, right, ord.compare(_, _) > 0) {
    def mapArgs(f: EndoFunction[Expr[_]]) = GT(
      f(left), f(right))
    override def toString = left.toString + " > " + right.toString
  }

  case class IfElse[T <: Numeric[T]]
    (condition: Expr[Boolean], left: Expr[T], right: Expr[T])(implicit num: NumericOps[T]) extends Expr[T] {

    val args = List(condition, left, right)
    def derivative(v: Var[T]) = IfElse(condition, left.derivative(v), right.derivative(v))
    def eval(f: Any => Any) = if (condition.eval(f)) left.eval(f) else right.eval(f)
    def mapArgs(f: EndoFunction[Expr[_]]) = IfElse(
      f(condition).asInstanceOf[Expr[Boolean]],
      f(left),
      f(right))
    override def toString = "if (" + condition + ")(" + left + ") else (" + right + ")"
    override lazy val hashCode = ScalaRunTime._hashCode(this);
  }

  object Expr {
    /** Creates a constant expression */
    def const[T](value: T)(implicit num: NumericOps[T]): Leaf[T] =
      if (num.zero == value) Zero[T]
      else Const(value)

    implicit def double2Constant[T](d: Double)(implicit num: NumericOps[T]): Leaf[T] =
      const(num.fromDouble(d))

    implicit def float2Constant[T](f: Float)(implicit num: NumericOps[T]): Leaf[T] =
      const(num.fromDouble(f.toDouble))

    implicit def int2Constant[T](i: Int)(implicit num: NumericOps[T]): Leaf[T] =
      const(num.fromDouble(i.toDouble))

    implicit def long2Constant[T](l: Long)(implicit num: NumericOps[T]): Leaf[T] =
      const(num.fromDouble(l.toDouble))
  }
}