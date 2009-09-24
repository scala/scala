//############################################################################
// Programmation IV - 2002 - Week 07
//############################################################################

object M0 {

  trait Expr {
    def isNumber: Boolean;
    def isSum: Boolean;
    def numValue: Int;
    def leftOp: Expr;
    def rightOp: Expr;
  }

  class Number(n: Int) extends Expr {
    def isNumber: Boolean = true;
    def isSum: Boolean = false;
    def numValue: Int = n;
    def leftOp: Expr = error("Number.leftOp");
    def rightOp: Expr = error("Number.rightOp");
  }
  class Sum(e1: Expr, e2: Expr) extends Expr {
    def isNumber: Boolean = false;
    def isSum: Boolean = true;
    def numValue: Int = error("Sum.numValue");
    def leftOp: Expr = e1;
    def rightOp: Expr = e2;
  }

  class Prod(e1: Expr, e2: Expr) extends Expr {
    def isNumber: Boolean = false;
    def isSum: Boolean = false;
    def numValue: Int = error("Prod.numValue");
    def leftOp: Expr = e1;
    def rightOp: Expr = e2;
  }

  class Var(x: String) extends Expr {
    def isNumber: Boolean = false;
    def isSum: Boolean = false;
    def numValue: Int = error("Var.numValue");
    def leftOp: Expr = error("Var.leftOp");
    def rightOp: Expr = error("Var.rightOp");
  }

  def eval(e: Expr): Int = {
    if (e.isNumber) e.numValue
    else if (e.isSum) eval(e.leftOp) + eval(e.rightOp)
    else error("unknown expression")
  }

  def test = {
    Console.println("        0 = " + eval(new Number(0)));
    Console.println("        1 = " + eval(new Number(1)));
    Console.println("    0 + 1 = " +
      eval(new Sum(new Number(0),new Number(1))));
    Console.println("    1 + 2 = " +
      eval(new Sum(new Number(1),new Number(2))));
    Console.println("2 + 3 + 4 = " +
      eval(new Sum(new Sum(new Number(2),new Number(3)),new Number(4))));
    Console.println;
  }

}

//############################################################################

object M1 {

  trait Expr {
    def eval: Int;
  }
  class Number(n: Int) extends Expr {
    def eval: Int = n;
  }
  class Sum(e1: Expr, e2: Expr) extends Expr {
    def eval: Int = e1.eval + e2.eval;
  }

  def test = {
    Console.println("        0 = " + new Number(0).eval);
    Console.println("        1 = " + new Number(1).eval);
    Console.println("    0 + 1 = " +
      new Sum(new Number(0),new Number(1)).eval);
    Console.println("    1 + 2 = " +
      new Sum(new Number(1),new Number(2)).eval);
    Console.println("2 + 3 + 4 = " +
      new Sum(new Sum(new Number(2),new Number(3)),new Number(4)).eval);
    Console.println;
  }
}

//############################################################################

object M2 {

  trait Expr;
  case class Number(n: Int) extends Expr;
  case class Sum(e1: Expr, e2: Expr) extends Expr;

  def eval(e: Expr): Int = e match {
    case Number(n) => n
    case Sum(e1, e2) => eval(e1) + eval(e2)
  }

  def test = {
    Console.println("        0 = " + eval(Number(0)));
    Console.println("        1 = " + eval(Number(1)));
    Console.println("    0 + 1 = " + eval(Sum(Number(0),Number(1))));
    Console.println("    1 + 2 = " + eval(Sum(Number(1),Number(2))));
    Console.println("2 + 3 + 4 = " + eval(Sum(Sum(Number(2),Number(3)),
                                             Number(4))));
    Console.println;
  }
}

//############################################################################

object M3 {

  trait Expr {
    def eval: Int = this match {
      case Number(n) => n
      case Sum(e1, e2) => e1.eval + e2.eval
    }
  }
  case class Number(n: Int) extends Expr;
  case class Sum(e1: Expr, e2: Expr) extends Expr;

  def test = {
    Console.println("        0 = " + Number(0).eval);
    Console.println("        1 = " + Number(1).eval);
    Console.println("    0 + 1 = " + Sum(Number(0),Number(1)).eval);
    Console.println("    1 + 2 = " + Sum(Number(1),Number(2)).eval);
    Console.println("2 + 3 + 4 = " + Sum(Sum(Number(2),Number(3)),
                                             Number(4)).eval);
    Console.println;
  }

}

//############################################################################

object M4 {

  def concat[a](xss: List[List[a]]): List[a] = xss match {
    case List() => List()
    case xs :: xss1 => xs ::: concat(xss1)
  }

  def test_concat[a](xss: List[List[a]]) = {
    Console.println(concat(xss).toString + " = concat(" + xss + ")"); // !!! .toString
  }

  def test = {
    test_concat(List());
    test_concat(List(List()));
    test_concat(List(List(),List()));
    test_concat(List(List(),List(),List()));

    test_concat(List(List(1,2,3,4,5,6)));
    test_concat(List(List(1,2,3,4,5,6),List[Int]())); // !!! [int]
    test_concat(List(List(1,2,3),List(4,5,6)));
    test_concat(List(List[Int](),List(1,2,3,4,5,6))); // !!! [int]
    test_concat(List(List(1,2,3,4,5,6),List[Int](),List[Int]())); // !!! [int]
    test_concat(List(List(1,2,3,4,5),List(6),List[Int]())); // !!! [int]
    test_concat(List(List(1,2,3),List(4,5,6),List[Int]())); // !!! [int]
    test_concat(List(List(1),List(2,3,4,5,6),List[Int]())); // !!! [int]
    test_concat(List(List[Int](),List(1,2,3,4,5,6),List[Int]())); // !!! [int]
    test_concat(List(List[Int](),List(1,2,3,4,5),List(6))); // !!! [int]
    test_concat(List(List[Int](),List(1,2,3),List(4,5,6))); // !!! [int]
    test_concat(List(List[Int](),List(1),List(2,3,4,5,6))); // !!! [int]
    test_concat(List(List[Int](),List[Int](),List(1,2,3,4,5,6))); // !!! [int]
    test_concat(List(List(1,2),List(3,4),List(5,6)));
    Console.println;
  }

}

//############################################################################

object M5 {

  def zipFun[a,b](xs:List[a], ys:List[b]):List[Pair[a,b]] = Pair(xs,ys) match {
    case Pair(List(), _) => List()
    case Pair(_, List()) => List()
    case Pair(x :: xs1, y :: ys1) => Pair(x, y) :: zipFun(xs1, ys1)
  }

  def test_zipFun[a,b](xs: List[a], ys: List[b]) = {
    Console.println(zipFun(xs,ys).toString + " = zipFun(" + xs + "," + ys + ")"); // !!! .toString
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

    Console.println;
  }

}


//############################################################################

object M6 {

  def zipFun[a,b](xs:List[a], ys:List[b]):List[Pair[a,b]] = (Pair(xs,ys): @unchecked) match {
    // !!! case Pair(List(), _), Pair(_, List()) => List()
    case Pair(x :: xs1, y :: ys1) => Pair(x, y) :: zipFun(xs1, ys1)
  }

  def test_zipFun[a,b](xs: List[a], ys: List[b]) = {
    Console.println(zipFun(xs,ys).toString + " = zipFun(" + xs + "," + ys + ")"); // !!! .toString
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

    Console.println;
  }

}

//############################################################################

object M7 {

  def heads[a](xss: List[List[a]]): List[a] = xss flatMap {
    case x :: xs => List(x)
    case List() => List()
  }

  def test_heads[a](xss: List[List[a]]) = {
    Console.println(heads(xss).toString + " = heads(" + xss + ")"); // !!! .toString
  }

  def test = {
    test_heads(List());
    test_heads(List(List()));
    test_heads(List(List(),List()));
    test_heads(List(List(),List(),List()));

    test_heads(List(List(1,2,3,4,5,6)));
    test_heads(List(List(1,2,3,4,5,6),List[Int]())); // !!! [int]
    test_heads(List(List[Int](),List(1,2,3,4,5,6))); // !!! [int]
    test_heads(List(List(1,2,3,4,5,6),List[Int](),List[Int]())); // !!! [int]
    test_heads(List(List[Int](),List(1,2,3,4,5,6),List[Int]())); // !!! [int]
    test_heads(List(List[Int](),List[Int](),List(1,2,3,4,5,6))); // !!! [int]

    test_heads(List(List(1),List(2,3,4,5,6),List[Int]())); // !!! [int]
    test_heads(List(List[Int](),List(1),List(2,3,4,5,6))); // !!! [int]

    test_heads(List(List(1,2,3),List(4,5,6)));
    test_heads(List(List(1,2,3),List(4,5,6),List[Int]())); // !!! [int]
    test_heads(List(List[Int](),List(1,2,3),List(4,5,6))); // !!! [int]

    test_heads(List(List(1,2,3,4,5),List(6),List[Int]())); // !!! [int]
    test_heads(List(List[Int](),List(1,2,3,4,5),List(6))); // !!! [int]

    test_heads(List(List(1,2),List(3,4),List(5,6)));

    Console.println;
  }

}

//############################################################################

object M8 {

  def heads[a](xss: List[List[a]]): List[a] = xss.flatMap {
    y => y match {
      case x :: xs => List(x)
      case List() => List()
    }
  }

  def test_heads[a](xss: List[List[a]]) = {
    Console.println(heads(xss).toString + " = heads(" + xss + ")"); // !!! .toString
  }


  def test = {
    test_heads(List());
    test_heads(List(List()));
    test_heads(List(List(),List()));
    test_heads(List(List(),List(),List()));

    test_heads(List(List(1,2,3,4,5,6)));
    test_heads(List(List(1,2,3,4,5,6),List[Int]())); // !!! [int]
    test_heads(List(List[Int](),List(1,2,3,4,5,6))); // !!! [int]
    test_heads(List(List(1,2,3,4,5,6),List[Int](),List[Int]())); // !!! [int]
    test_heads(List(List[Int](),List(1,2,3,4,5,6),List[Int]())); // !!! [int]
    test_heads(List(List[Int](),List[Int](),List(1,2,3,4,5,6))); // !!! [int]

    test_heads(List(List(1),List(2,3,4,5,6),List[Int]())); // !!! [int]
    test_heads(List(List[Int](),List(1),List(2,3,4,5,6))); // !!! [int]

    test_heads(List(List(1,2,3),List(4,5,6)));
    test_heads(List(List(1,2,3),List(4,5,6),List[Int]())); // !!! [int]
    test_heads(List(List[Int](),List(1,2,3),List(4,5,6))); // !!!

    test_heads(List(List(1,2,3,4,5),List(6),List[Int]())); // !!! [int]
    test_heads(List(List[Int](),List(1,2,3,4,5),List(6))); // !!! [int]

    test_heads(List(List(1,2),List(3,4),List(5,6)));

    Console.println;
  }

}

//############################################################################

object M9 {

  trait Expr {
    def derive(v: Var): Expr = this match {
      case Number(_) => Number(0)
      case Var(name) => if (name == v.name) Number(1) else Number(0)
      case Sum(e1, e2) => Sum(e1 derive v, e2 derive v)
      case Prod(e1, e2) => Sum(Prod(e1, e2 derive v), Prod(e2, e1 derive v))
    }
  }
  case class Number(x: Int) extends Expr {
    override def toString = "Number(" + x + ")"; // !!! remove !
  }
  case class Var(name: String) extends Expr {
    override def toString = "Var(" + name + ")"; // !!! remove !
  }
  case class Sum(e1: Expr, e2: Expr) extends Expr {
    override def toString = "Sum(" + e1 + ", " + e2 + ")"; // !!! remove !
  }
  case class Prod(e1: Expr, e2: Expr) extends Expr {
    override def toString = "Prod(" + e1 + ", " + e2 + ")"; // !!! remove !
  }

  def test = {
    val x = Var("x");
    val f0 = Prod(x, x);
    val f1 = f0 derive x;
    Console.println("f (x) = " + f0);
    Console.println("f'(x) = " + f1);
    Console.println;
  }

}

//############################################################################

object MA {

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
  case class Number(x: Int) extends Expr {
    override def toString = x.toString
  }
  case class Var(name: String) extends Expr {
    override def toString = name;
  }
  case class Sum(e1: Expr, e2: Expr) extends Expr {
    override def toString = e1.toString + " + " + e2.toString;
  }
  case class Prod(e1: Expr, e2: Expr) extends Expr {
    override def toString = {
      def factorToString(e: Expr) = e match {
        case Sum(_, _) => "(" + e.toString + ")"
        case _ => e.toString
      }
      factorToString(e1) + " * " + factorToString(e2);
    }
  }

  def eval(e: Expr): Int = e match {
    case Number(n) => n
    case Var(_) => error("cannot evaluate variable")
    case Sum(e1, e2) => eval(e1) + eval(e2)
    case Prod(e1, e2) => eval(e1) * eval(e2)
  }

  def evalvars(xs: List[(String,Int)]): Expr => Int = {
    def loop(e: Expr): Int = e match {
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
    Console.println("f (x) = " + f0);
    Console.println("f'(x) = " + f1);

    val g0 = Number(2) * x * x + Number(3) * x;
    val g1 = g0 derive x;
    Console.println("g (x) = " + g0);
    Console.println("g'(x) = " + g1);
    Console.println("g (3) = " + evalvars(List(Pair("x",3)))(g0));
    Console.println("g'(3) = " + evalvars(List(Pair("x",3)))(g1));

    Console.println;
  }

}

//############################################################################

object Utils {

  private def power0(x: Int, y: Int): Int =
    if (y == 1) x else if (y % 2 == 0) power0(x*x,y/2) else x*power0(x, y-1);

  def power(x: Int, y: Int): Int = (x,y) match {
    case Pair(0,0) => error("power(0,0)")
    case Pair(0,_) => 0
    case Pair(1,_) => 1
    case Pair(_,0) => 1
    case Pair(_,1) => x
    case Pair(_,2) => x*x
    case Pair(_,_) => if (y < 0) 1/power0(x,y) else power0(x,y)
  }

  def lookup(entries: List[(String,Int)], key: String): Int = entries match {
    case List()                       => error("no value for " + key)
    case Pair(k,v) :: _ if (k == key) => v
    case _ :: rest                    => lookup(rest, key)
  }

  def compare(xs: List[String], ys: List[String]): Int = (xs, ys) match {
    case Pair(List(), List()) =>  0
    case Pair(List(), _     ) => -1
    case Pair(_     , List()) => +1
    case Pair(x::xs , y::ys ) => {
      val diff = x.compareTo(y);
      if (diff != 0) diff else compare(xs,ys)
    }
  }

}

object MB {

  import Utils._;


  trait Expr {

    private def count: Int = this match {
      case Lit(n)        => n
      case Mul(Lit(n),_) => n
      case _             => 1
    }

    private def term: Expr = this match {
      case Lit(_)        => Lit(1)
      case Mul(Lit(_),r) => r
      case _             => this
    }

    private def vars: List[String] = this match {
      case Var(n)   => List(n)
      case Mul(l,r) => l.vars ::: r.vars
      case Pow(l,n) => { val vs = l.vars; List.range(0,n).flatMap(i => vs) }
      case _        => List()
    }

    private def +<  (that: Expr): Boolean = (this +<? that) <  0;
    private def +<= (that: Expr): Boolean = (this +<? that) <= 0;
    private def +<? (that: Expr): Int = Pair(this,that) match {
      case Pair(Add(_,_), _       ) =>  0
      case Pair(_       , Add(_,_)) =>  0
      case Pair(_       , _       ) => compare(this.vars,that.vars)
    }

    def + (that: Expr): Expr = if (that +<= this) Pair(this,that) match {
      case Pair(_         , Lit(0)    )                  => this
      case Pair(Lit(l)    , Lit(r)    )                  => Lit(l + r)
      case Pair(_         , Add(rl,rr))                  => (this + rl) + rr
      case Pair(Add(ll,lr), _         ) if (lr +<= that) => ll + (that + lr)
      case Pair(_         , _         )                  => {
        val l = this.term;
        val r = that.term;
        if (l equ r) Lit(this.count + that.count) * r else Add(this, that)
      }
    } else that + this;

    private def *<  (that: Expr): Boolean = (this *<? that) <  0;
    private def *<= (that: Expr): Boolean = (this *<? that) <= 0;
    private def *<? (that: Expr): Int = Pair(this,that) match {
      case Pair(Mul(_,_), _       ) =>  0
      case Pair(_       , Mul(_,_)) =>  0
      case Pair(Add(_,_), Add(_,_)) =>  0
      case Pair(Add(_,_), _       ) => -1
      case Pair(_       , Add(_,_)) => +1
      case Pair(Lit(_)  , Lit(_)  ) =>  0
      case Pair(Lit(_)  , _       ) => -1
      case Pair(_       , Lit(_)  ) => +1
      case Pair(Var(l)  , Var(r)  ) => l.compareTo(r)
      case Pair(Var(_)  , Pow(r,_)) => if (this *<= r) -1 else +1
      case Pair(Pow(l,_), Var(_)  ) => if (l *<  that) -1 else +1
      case Pair(Pow(l,_), Pow(r,_)) => l *<? r
    }

    def * (that: Expr): Expr = if (this *<= that) Pair(this,that) match {
      case Pair(Lit(0)    , _         )                    => this
      case Pair(Lit(1)    , _         )                    => that
      case Pair(Mul(ll,lr), r         )                    => ll * (lr * r)
      case Pair(Add(ll,lr), r         )                    => ll * r + lr * r
      case Pair(Lit(l)    , Lit(r)    )                    => Lit(l * r)
      case Pair(Var(_)    , Var(_)    ) if (this equ that) => Pow(this,2)
      case Pair(Var(_)    , Pow(r,n)  ) if (this equ r)    => Pow(this,n + 1)
      case Pair(Pow(ll,lr), Pow(rl,rr)) if (ll equ rl)     => Pow(ll,lr + rr)
      case Pair(l         , Mul(rl,rr)) if (rl *<= l)      => (rl * l) * rr
      case Pair(_         , _         )                    => Mul(this,that)
    } else that * this;

    def ^ (that: Int): Expr = (this,that) match {
      case Pair(_       ,1) => this
      case Pair(Lit(i)  ,n) => Lit(power(i,n))
      case Pair(Var(_)  ,n) => Pow(this,n)
      case Pair(Add(_,_),n) => this * (this ^ (n - 1))
      case Pair(Mul(l,r),n) => (l ^ n) * (r ^ n)
      case Pair(Pow(e,m),n) => Pow(e,m + n)
    }

    def derive(v: Var): Expr = this match {
      case Lit(_) => Lit(0)
      case Var(name) => if (name == v.name) Lit(1) else Lit(0)
      case Add(e1, e2) => (e1 derive v) + (e2 derive v)
      case Mul(e1, e2) => e1 * (e2 derive v) + e2 * (e1 derive v)
      case Pow(e1, i2) => Lit(i2) * (e1 derive v) * (e1 ^ (i2 - 1))
    }

    def evaluate(vars: List[(String,Int)]): Int = this match {
      case Lit(cst) => cst
      case Var  (name) => lookup(vars, name)
      case Add  (l, r) => l.evaluate(vars) + r.evaluate(vars)
      case Mul (l, r) => l.evaluate(vars) * r.evaluate(vars)
      case Pow(l, r) => power(l.evaluate(vars), r)
    }

    def equ(that: Expr): Boolean = Pair(this,that) match {
      case Pair(Lit(l)    ,Lit(r))     => l == r
      case Pair(Var(l)    ,Var(r))     => l == r
      case Pair(Add(ll,lr),Add(rl,rr)) => (ll equ rl) && (lr equ rr)
      case Pair(Mul(ll,lr),Mul(rl,rr)) => (ll equ rl) && (lr equ rr)
      case Pair(Pow(ll,lr),Pow(rl,rr)) => (ll equ rl) && (lr == rr)
      case _ => false
    }

  }

  case class Lit(x: Int) extends Expr {
    override def toString = x.toString
  }

  case class Var(name: String) extends Expr {
    override def toString = name;
  }

  case class Add(e1: Expr, e2: Expr) extends Expr {
    override def toString = e1.toString + " + " + e2.toString; // !!! .toString
  }

  case class Mul(e1: Expr, e2: Expr) extends Expr {
    override def toString = {
      def factorToString(e: Expr) = e match {
        case Add(_, _) => "(" + e.toString + ")"
        case _         =>       e.toString
      }
      factorToString(e1) + " * " + factorToString(e2);
    }
  }

  case class Pow(e1: Expr, i2: Int) extends Expr {
    override def toString = {
      def factorToString(e: Expr) = e match {
        case Add(_, _) => "(" + e.toString + ")"
        case Mul(_, _) => "(" + e.toString + ")"
        case _         =>       e.toString
      }
      factorToString(e1) + "^" + i2;
    }
  }

  def test = {
    val _1 = Lit(1);
    val _2 = Lit(2);
    val _3 = Lit(3);
    val _4 = Lit(4);
    val _5 = Lit(5);

    val x  = Var("x");

    val ta = (_1 + (_2 + x));
    val tb = (_1 + (x + _2));
    val tc = ((_1 + x) + _2);
    val td = ((x + _1) + _2);
    val te = ((x + _1) + (x + _2));
    val tf = ((_1 + x) + (_2 + x));
    val tg = x + x + (x * _2) + x + x;
    val th = x * x * (x ^  2) * x * x;

    Console.println("ta(x) = " + ta);
    Console.println("tb(x) = " + tb);
    Console.println("tc(x) = " + tc);
    Console.println("td(x) = " + td);
    Console.println("te(x) = " + te);
    Console.println("tf(x) = " + tf);
    Console.println("tg(x) = " + tg);
    Console.println("th(x) = " + th);
    Console.println;

    val f4 = (x+ _3)*(_2+x)*x*(x+ _1) + (x+ _5)*(x*(x+ _2)+x+ _1) + (x^2) + x;
    val f3 = f4.derive(x);
    val f2 = f3.derive(x);
    val f1 = f2.derive(x);
    val f0 = f1.derive(x);

    Console.println("f4(x) = " + f4);
    Console.println("f3(x) = " + f3);
    Console.println("f2(x) = " + f2);
    Console.println("f1(x) = " + f1);
    Console.println("f0(x) = " + f0);
    Console.println;

    def check(n: String, f: Expr, x: Int, e: Int) {
      val a: Int = f.evaluate(List(Pair("x",x)));
      val s: String = if (a == e) "ok" else "KO(" + e + ")";
      Console.println(n + "(" + x + ") = " + a + " " + s);
    }

    check("f4", f4, 0, 5);
    check("f4", f4, 1, 56);
    check("f4", f4, 2, 203);
    check("f4", f4, 3, 524);
    check("f4", f4, 4, 1121);
    Console.println;

    check("f3", f3, 0, 23);
    check("f3", f3, 1, 88);
    check("f3", f3, 2, 219);
    check("f3", f3, 3, 440);
    Console.println;

    check("f2", f2, 0, 40);
    check("f2", f2, 1, 94);
    check("f2", f2, 2, 172);
    Console.println;

    check("f1", f1, 0, 42);
    check("f1", f1, 1, 66);
    Console.println;

    check("f0", f0, 0, 24);
    Console.println;
  }
}

//############################################################################

object Test {
  def main(args: Array[String]) {
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
    MB.test;
    ()
  }
}

//############################################################################
