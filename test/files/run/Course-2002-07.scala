//############################################################################
// Programmation IV - 2002 - Week 07
//############################################################################
// $Id$

module M0 {

  trait Expr {
    def isNumber: boolean;
    def isSum: boolean;
    def numValue: int;
    def leftOp: Expr;
    def rightOp: Expr;
  }

  class Number(n: int) extends Expr {
    def isNumber: boolean = true;
    def isSum: boolean = false;
    def numValue: int = n;
    def leftOp: Expr = error("Number.leftOp");
    def rightOp: Expr = error("Number.rightOp");
  }
  class Sum(e1: Expr, e2: Expr) extends Expr {
    def isNumber: boolean = false;
    def isSum: boolean = true;
    def numValue: int = error("Sum.numValue");
    def leftOp: Expr = e1;
    def rightOp: Expr = e2;
  }

  class Prod(e1: Expr, e2: Expr) extends Expr {
    def isNumber: boolean = false;
    def isSum: boolean = false;
    def numValue: int = error("Prod.numValue");
    def leftOp: Expr = e1;
    def rightOp: Expr = e2;
  }

  class Var(x: String) extends Expr {
    def isNumber: boolean = false;
    def isSum: boolean = false;
    def numValue: int = error("Var.numValue");
    def leftOp: Expr = error("Var.leftOp");
    def rightOp: Expr = error("Var.rightOp");
  }

  def eval(e: Expr): int = {
    if (e.isNumber) e.numValue
    else if (e.isSum) eval(e.leftOp) + eval(e.rightOp)
    else error("unknown expression")
  }

  def test = {
    System.out.println("        0 = " + eval(new Number(0)));
    System.out.println("        1 = " + eval(new Number(1)));
    System.out.println("    0 + 1 = " +
      eval(new Sum(new Number(0),new Number(1))));
    System.out.println("    1 + 2 = " +
      eval(new Sum(new Number(1),new Number(2))));
    System.out.println("2 + 3 + 4 = " +
      eval(new Sum(new Sum(new Number(2),new Number(3)),new Number(4))));
    System.out.println();
  }

}

//############################################################################

module M1 {

  trait Expr {
    def eval: int;
  }
  class Number(n: int) extends Expr {
    def eval: int = n;
  }
  class Sum(e1: Expr, e2: Expr) extends Expr {
    def eval: int = e1.eval + e2.eval;
  }

  def test = {
    System.out.println("        0 = " + new Number(0).eval);
    System.out.println("        1 = " + new Number(1).eval);
    System.out.println("    0 + 1 = " +
      new Sum(new Number(0),new Number(1)).eval);
    System.out.println("    1 + 2 = " +
      new Sum(new Number(1),new Number(2)).eval);
    System.out.println("2 + 3 + 4 = " +
      new Sum(new Sum(new Number(2),new Number(3)),new Number(4)).eval);
    System.out.println();
  }
}

//############################################################################

module M2 {

  trait Expr;
  case class Number(n: int) extends Expr;
  case class Sum(e1: Expr, e2: Expr) extends Expr;

  def eval(e: Expr): int = e match {
    case Number(n) => n
    case Sum(e1, e2) => eval(e1) + eval(e2)
  }

  def test = {
    System.out.println("        0 = " + eval(Number(0)));
    System.out.println("        1 = " + eval(Number(1)));
    System.out.println("    0 + 1 = " + eval(Sum(Number(0),Number(1))));
    System.out.println("    1 + 2 = " + eval(Sum(Number(1),Number(2))));
    System.out.println("2 + 3 + 4 = " + eval(Sum(Sum(Number(2),Number(3)),
                                             Number(4))));
    System.out.println();
  }
}

//############################################################################

module M3 {

  trait Expr {
    def eval: int = this match {
      case Number(n) => n
      case Sum(e1, e2) => e1.eval + e2.eval
    }
  }
  case class Number(n: int) extends Expr;
  case class Sum(e1: Expr, e2: Expr) extends Expr;

  def test = {
    System.out.println("        0 = " + Number(0).eval);
    System.out.println("        1 = " + Number(1).eval);
    System.out.println("    0 + 1 = " + Sum(Number(0),Number(1)).eval);
    System.out.println("    1 + 2 = " + Sum(Number(1),Number(2)).eval);
    System.out.println("2 + 3 + 4 = " + Sum(Sum(Number(2),Number(3)),
                                             Number(4)).eval);
    System.out.println();
  }

}

//############################################################################

module M4 {

  def concat[a](xss: List[List[a]]): List[a] = xss match {
    case List() => List()
    case xs :: xss1 => xs ::: concat(xss1)
  }

  def test_concat[a](xss: List[List[a]]) = {
    System.out.println(concat(xss).toString() + " = concat(" + xss + ")"); // !!! .toString()
  }

  def test = {
    test_concat(List());
    test_concat(List(List()));
    test_concat(List(List(),List()));
    test_concat(List(List(),List(),List()));

    test_concat(List(List(1,2,3,4,5,6)));
    test_concat(List(List(1,2,3,4,5,6),List[int]())); // !!! [int]
    test_concat(List(List(1,2,3),List(4,5,6)));
    test_concat(List(List[int](),List(1,2,3,4,5,6))); // !!! [int]
    test_concat(List(List(1,2,3,4,5,6),List[int](),List[int]())); // !!! [int]
    test_concat(List(List(1,2,3,4,5),List(6),List[int]())); // !!! [int]
    test_concat(List(List(1,2,3),List(4,5,6),List[int]())); // !!! [int]
    test_concat(List(List(1),List(2,3,4,5,6),List[int]())); // !!! [int]
    test_concat(List(List[int](),List(1,2,3,4,5,6),List[int]())); // !!! [int]
    test_concat(List(List[int](),List(1,2,3,4,5),List(6))); // !!! [int]
    test_concat(List(List[int](),List(1,2,3),List(4,5,6))); // !!! [int]
    test_concat(List(List[int](),List(1),List(2,3,4,5,6))); // !!! [int]
    test_concat(List(List[int](),List[int](),List(1,2,3,4,5,6))); // !!! [int]
    test_concat(List(List(1,2),List(3,4),List(5,6)));
    System.out.println();
  }

}

//############################################################################

module M5 {

  def zipFun[a,b](xs:List[a], ys:List[b]):List[Pair[a,b]] = Pair(xs,ys) match {
    case Pair(List(), _) => List()
    case Pair(_, List()) => List()
    case Pair(x :: xs1, y :: ys1) => Pair(x, y) :: zipFun(xs1, ys1)
  }

  def test_zipFun[a,b](xs: List[a], ys: List[b]) = {
    System.out.println(zipFun(xs,ys).toString() + " = zipFun(" + xs + "," + ys + ")"); // !!! .toString()
  }

  def test = {
    test_zipFun(List(),List());
    test_zipFun(List(),List('a','b','c'));
    test_zipFun(List(1,2,3),List());

    test_zipFun(List(1),List('a'));
    test_zipFun(List(1),List('a','b','c'));
    test_zipFun(List(1,2,3),List('a'));

    test_zipFun(List(1,2),List('a','b'));
    test_zipFun(List(1,2),List('a','b','c'));
    test_zipFun(List(1,2,3),List('a','b'));

    test_zipFun(List(1,2,3),List('a','b','c'));

    System.out.println();
  }

}


//############################################################################

module M6 {

  def zipFun[a,b](xs:List[a], ys:List[b]):List[Pair[a,b]] = Pair(xs,ys) match {
    // !!! case Pair(List(), _), Pair(_, List()) => List()
    case Pair(x :: xs1, y :: ys1) => Pair(x, y) :: zipFun(xs1, ys1)
  }

  def test_zipFun[a,b](xs: List[a], ys: List[b]) = {
    System.out.println(zipFun(xs,ys).toString() + " = zipFun(" + xs + "," + ys + ")"); // !!! .toString()
  }

  def test = {
    test_zipFun(List(),List());
    test_zipFun(List(),List('a','b','c'));
    test_zipFun(List(1,2,3),List());

    test_zipFun(List(1),List('a'));
    test_zipFun(List(1),List('a','b','c'));
    test_zipFun(List(1,2,3),List('a'));

    test_zipFun(List(1,2),List('a','b'));
    test_zipFun(List(1,2),List('a','b','c'));
    test_zipFun(List(1,2,3),List('a','b'));

    test_zipFun(List(1,2,3),List('a','b','c'));

    System.out.println();
  }

}

//############################################################################

module M7 {

  def heads[a](xss: List[List[a]]): List[a] = xss flatMap {
    case x :: xs => List(x)
    case List() => List()
  }

  def test_heads[a](xss: List[List[a]]) = {
    System.out.println(heads(xss).toString() + " = heads(" + xss + ")"); // !!! .toString()
  }


  def test = {
    test_heads(List());
    test_heads(List(List()));
    test_heads(List(List(),List()));
    test_heads(List(List(),List(),List()));

    test_heads(List(List(1,2,3,4,5,6)));
    test_heads(List(List(1,2,3,4,5,6),List[int]())); // !!! [int]
    test_heads(List(List[int](),List(1,2,3,4,5,6))); // !!! [int]
    test_heads(List(List(1,2,3,4,5,6),List[int](),List[int]())); // !!! [int]
    test_heads(List(List[int](),List(1,2,3,4,5,6),List[int]())); // !!! [int]
    test_heads(List(List[int](),List[int](),List(1,2,3,4,5,6))); // !!! [int]

    test_heads(List(List(1),List(2,3,4,5,6),List[int]())); // !!! [int]
    test_heads(List(List[int](),List(1),List(2,3,4,5,6))); // !!! [int]

    test_heads(List(List(1,2,3),List(4,5,6)));
    test_heads(List(List(1,2,3),List(4,5,6),List[int]())); // !!! [int]
    test_heads(List(List[int](),List(1,2,3),List(4,5,6))); // !!! [int]

    test_heads(List(List(1,2,3,4,5),List(6),List[int]())); // !!! [int]
    test_heads(List(List[int](),List(1,2,3,4,5),List(6))); // !!! [int]

    test_heads(List(List(1,2),List(3,4),List(5,6)));

    System.out.println();
  }

}

//############################################################################

module M8 {

  def heads[a](xss: List[List[a]]): List[a] = xss.flatMap {
    y => y match {
      case x :: xs => List(x)
      case List() => List()
    }
  }

  def test_heads[a](xss: List[List[a]]) = {
    System.out.println(heads(xss).toString() + " = heads(" + xss + ")"); // !!! .toString()
  }


  def test = {
    test_heads(List());
    test_heads(List(List()));
    test_heads(List(List(),List()));
    test_heads(List(List(),List(),List()));

    test_heads(List(List(1,2,3,4,5,6)));
    test_heads(List(List(1,2,3,4,5,6),List[int]())); // !!! [int]
    test_heads(List(List[int](),List(1,2,3,4,5,6))); // !!! [int]
    test_heads(List(List(1,2,3,4,5,6),List[int](),List[int]())); // !!! [int]
    test_heads(List(List[int](),List(1,2,3,4,5,6),List[int]())); // !!! [int]
    test_heads(List(List[int](),List[int](),List(1,2,3,4,5,6))); // !!! [int]

    test_heads(List(List(1),List(2,3,4,5,6),List[int]())); // !!! [int]
    test_heads(List(List[int](),List(1),List(2,3,4,5,6))); // !!! [int]

    test_heads(List(List(1,2,3),List(4,5,6)));
    test_heads(List(List(1,2,3),List(4,5,6),List[int]())); // !!! [int]
    test_heads(List(List[int](),List(1,2,3),List(4,5,6))); // !!!

    test_heads(List(List(1,2,3,4,5),List(6),List[int]())); // !!! [int]
    test_heads(List(List[int](),List(1,2,3,4,5),List(6))); // !!! [int]

    test_heads(List(List(1,2),List(3,4),List(5,6)));

    System.out.println();
  }

}

//############################################################################

module M9 {

  trait Expr {
    def derive(v: Var): Expr = match {
      case Number(_) => Number(0)
      case Var(name) => if (name == v.name) Number(1) else Number(0)
      case Sum(e1, e2) => Sum(e1 derive v, e2 derive v)
      case Prod(e1, e2) => Sum(Prod(e1, e2 derive v), Prod(e2, e1 derive v))
    }
  }
  case class Number(x: int) extends Expr {
    override def toString() = "Number(" + x + ")"; // !!! remove !
  }
  case class Var(name: String) extends Expr {
    override def toString() = "Var(" + name + ")"; // !!! remove !
  }
  case class Sum(e1: Expr, e2: Expr) extends Expr {
    override def toString() = "Sum(" + e1 + ", " + e2 + ")"; // !!! remove !
  }
  case class Prod(e1: Expr, e2: Expr) extends Expr {
    override def toString() = "Prod(" + e1 + ", " + e2 + ")"; // !!! remove !
  }

  def test = {
    val x = Var("x");
    val f0 = Prod(x, x);
    val f1 = f0 derive x;
    System.out.println("f (x) = " + f0);
    System.out.println("f'(x) = " + f1);
    System.out.println();
  }

}

//############################################################################

module MA {

  def lookup[k,v](xs: List[Pair[k,v]], k: k): v = xs match {
    case List() => error("no value for " + k)
    case Pair(k1,v1) :: xs1 => if (k1 == k) v1 else lookup(xs1, k)
  }

  trait Expr {
    def + (that: Expr) = Sum(this, that);
    def * (that: Expr) = Prod(this, that);
    def derive(v: Var): Expr = this match {
      case Number(_) => Number(0)
      case Var(name) => if (name == v.name) Number(1) else Number(0)
      case Sum(e1, e2) => (e1 derive v) + (e2 derive v)
      case Prod(e1, e2) => e1 * (e2 derive v) + e2 * (e1 derive v)
    }
  }
  case class Number(x: int) extends Expr {
    override def toString() = x.toString()
  }
  case class Var(name: String) extends Expr {
    override def toString() = name;
  }
  case class Sum(e1: Expr, e2: Expr) extends Expr {
    override def toString() = e1.toString() + " + " + e2.toString();
  }
  case class Prod(e1: Expr, e2: Expr) extends Expr {
    override def toString() = {
      def factorToString(e: Expr) = e match {
        case Sum(_, _) => "(" + e.toString() + ")"
        case _ => e.toString()
      }
      factorToString(e1) + " * " + factorToString(e2);
    }
  }

  def eval(e: Expr): int = e match {
    case Number(n) => n
    case Var(_) => error("cannot evaluate variable")
    case Sum(e1, e2) => eval(e1) + eval(e2)
    case Prod(e1, e2) => eval(e1) * eval(e2)
  }

  def evalvars(xs: List[Pair[String,int]]) = {
    def loop(e: Expr): int = e match {
      case Number(n) => n
      case Var(name) => lookup(xs,name)
      case Sum(e1, e2) => loop(e1) + loop(e2)
      case Prod(e1, e2) => loop(e1) * loop(e2)
    }
    loop
  }

  def test = {
    val x = Var("x");

    val f0 = x * x;
    val f1 = f0 derive x;
    System.out.println("f (x) = " + f0);
    System.out.println("f'(x) = " + f1);

    val g0 = Number(2) * x * x + Number(3) * x;
    val g1 = g0 derive x;
    System.out.println("g (x) = " + g0);
    System.out.println("g'(x) = " + g1);
    System.out.println("g (3) = " + evalvars(List(Pair("x",3)))(g0));
    System.out.println("g'(3) = " + evalvars(List(Pair("x",3)))(g1));

    System.out.println();
  }

}

//############################################################################

module Test {
  def main(args: Array[String]): unit = {
    M0.test;
    M1.test;
    M2.test;
    M3.test;
    M4.test;
    M5.test;
    // !!! M6.test;
    M7.test;
    M8.test;
    M9.test;
    MA.test;
    ()
  }
}

//############################################################################
