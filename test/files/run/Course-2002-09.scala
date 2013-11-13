//############################################################################
// Programmation IV - 2002 - Week 09
//############################################################################

trait Constraint {
  def newValue: Unit;
  def dropValue: Unit
}

object NoConstraint extends Constraint {
  def newValue: Unit = sys.error("NoConstraint.newValue");
  def dropValue: Unit = sys.error("NoConstraint.dropValue");
}

class Adder(a1: Quantity,a2: Quantity,sum: Quantity) extends Constraint {
  def newValue = (a1.getValue, a2.getValue, sum.getValue) match {
    case (Some(x1), Some(x2), _      ) => sum.setValue(x1 + x2, this)
    case (Some(x1), _       , Some(r)) => a2.setValue(r - x1, this)
    case (_       , Some(x2), Some(r)) => a1.setValue(r - x2, this)
    case _                             =>
  }
  def dropValue: Unit = {
    a1.forgetValue(this); a2.forgetValue(this); sum.forgetValue(this);
  }
  a1 connect this;
  a2 connect this;
  sum connect this;
}

class Multiplier(m1: Quantity, m2: Quantity, prod: Quantity)
                extends Constraint {
  def newValue = (m1.getValue, m2.getValue, prod.getValue) match {
    case (Some(0d), _       , _      ) => prod.setValue(0, this);
    case (_       , Some(0d), _      ) => prod.setValue(0, this);
    case (Some(x1), Some(x2), _      ) => prod.setValue(x1 * x2, this)
    case (Some(x1), _       , Some(r)) => m2.setValue(r / x1, this)
    case (_,        Some(x2), Some(r)) => m1.setValue(r / x2, this)
    case _                             =>
  }
  def dropValue: Unit = {
    m1.forgetValue(this); m2.forgetValue(this); prod.forgetValue(this);
  }
  m1 connect this;
  m2 connect this;
  prod connect this;
}

class Squarer(square: Quantity, root: Quantity) extends Constraint {
  def newValue: Unit = (square.getValue, root.getValue) match {
    case (Some(x), _      )if (x < 0) => sys.error("Square of negative number")
    case (Some(x), _      )           => root.setValue(Math.sqrt(x), this)
    case (_      , Some(x))           => square.setValue(x*x, this)
    case _                            =>
  }
  def dropValue: Unit = {
    square.forgetValue(this); root.forgetValue(this);
  }
  square connect this;
  root connect this;
}

class Eq(a: Quantity, b: Quantity) extends Constraint {
  def newValue = ((a.getValue, b.getValue): @unchecked) match {
    case (Some(x), _      ) => b.setValue(x, this);
    case (_      , Some(y)) => a.setValue(y, this);
  }
  def dropValue {
    a.forgetValue(this); b.forgetValue(this);
  }
  a connect this;
  b connect this;
}

class Constant(q: Quantity, v: Double) extends Constraint {
  def newValue: Unit = sys.error("Constant.newValue");
  def dropValue: Unit = sys.error("Constant.dropValue");
  q connect this;
  q.setValue(v, this);
}

class Probe(name: String, q: Quantity) extends Constraint {
  def newValue: Unit = printProbe(q.getValue);
  def dropValue: Unit = printProbe(None);
  private def printProbe(v: Option[Double]) {
    val vstr = v match {
      case Some(x) => x.toString()
      case None => "?"
    }
    Console.println("Probe: " + name + " = " + vstr);
  }
  q connect this
}

class Quantity() {
  private var value: Option[Double] = None;
  private var constraints: List[Constraint] = List();
  private var informant: Constraint = null;

  def getValue: Option[Double] = value;

  def setValue(v: Double, setter: Constraint) = value match {
    case Some(v1) =>
      if (v != v1) sys.error("Error! contradiction: " + v + " and " + v1);
    case None =>
      informant = setter; value = Some(v);
      for (c <- constraints; if !(c == informant)) {
        c.newValue;
      }
  }
  def setValue(v: Double): Unit = setValue(v, NoConstraint);

  def forgetValue(retractor: Constraint): Unit = {
    if (retractor == informant) {
      value = None;
      for (c <- constraints; if !(c == informant)) c.dropValue;
    }
  }
  def forgetValue: Unit = forgetValue(NoConstraint);

  def connect(c: Constraint) = {
    constraints = c :: constraints;
    value match {
      case Some(_) => c.newValue
      case None =>
    }
  }

  def +(that: Quantity): Quantity = {
    val sum = new Quantity();
    new Adder(this, that, sum);
    sum;
  }

  def *(that: Quantity): Quantity = {
    val prod = new Quantity();
    new Multiplier(this, that, prod);
    prod;
  }

  def square: Quantity = {
    val square = new Quantity();
    new Squarer(square, this);
    square;
  }

  def sqrt: Quantity = {
    val root = new Quantity();
    new Squarer(this, root);
    root;
  }

  def ===(that: Quantity): Constraint = {
    new Eq(this, that);
  }

  override def toString(): String = value match {
    case None    => "  ?"
    case Some(v) => v.toString()
  }

  def str: String = toString();
}

//############################################################################

object M0 {

  def CFconverter(c: Quantity, f: Quantity) = {
    val u = new Quantity();
    val v = new Quantity();
    val w = new Quantity();
    val x = new Quantity();
    val y = new Quantity();
    new Multiplier(c, w, u);
    new Multiplier(v, x, u);
    new Adder(v, y, f);
    new Constant(w, 9);
    new Constant(x, 5);
    new Constant(y, 32);
  }

  def test = {
    val c = new Quantity(); new Probe("c", c);
    val f = new Quantity(); new Probe("f", f);
    CFconverter(c, f);

    c.setValue(0);
    c.forgetValue;
    Console.println;

    c.setValue(100);
    c.forgetValue;
    Console.println;

    f.setValue(32);
    f.forgetValue;
    Console.println;

    f.setValue(212);
    f.forgetValue;
    Console.println;
  }
}

//############################################################################

object M1 {

  def constant(x: Double): Quantity = {
    val q = new Quantity();
    new Constant(q, x);
    q
  }

  def CFconverter(c: Quantity, f: Quantity) = {
    val v = new Quantity();
    constant(9) * c === constant(5) * v;
    v + constant(32) === f;
  }

  def show_c2f(c: Quantity, f: Quantity, v: Int) = {
    c.setValue(v);
    Console.println(c.str + " Celsius -> " + f.str + " Fahrenheits");
    c.forgetValue;
  }

  def show_f2c(c: Quantity, f: Quantity, v: Int) = {
    f.setValue(v);
    Console.println(f.str + " Fahrenheits -> " + c.str + " Celsius");
    f.forgetValue;
  }

  def test = {
    val c = new Quantity();
    val f = new Quantity();
    CFconverter(c, f);

    show_c2f(c, f, 0);
    show_c2f(c, f, 100);
    show_f2c(c, f, 32);
    show_f2c(c, f, 212);
    Console.println;
  }
}

//############################################################################

object M2 {

  val a = new Quantity();
  val b = new Quantity();
  val c = a * b;

  def set(q: Quantity, o: Option[Int]): String = {
    o match {
      case None    => "?"
      case Some(v) => q.setValue(v); v.toString()
    };
  }

  def show(x: Option[Int], y: Option[Int], z: Option[Int]) = {
    Console.print("a = " +set(a,x)+ ", b = " +set(b,y)+ ", c = " +set(c,z));
    Console.println(" => " + a.str + " * " + b.str + " = " + c.str);
    a.forgetValue; b.forgetValue; c.forgetValue;
  }

  def test = {
    show(None , None , None );
    show(Some(2), None , None );
    show(None , Some(3), None );
    show(None , None , Some(6));
    show(Some(2), Some(3), None );
    show(Some(2), None , Some(6));
    show(None , Some(3), Some(6));
    show(Some(2), Some(3), Some(6));
    Console.println;

    show(Some(0), None , None );
    show(None , Some(0), None );
    show(None , None , Some(0));
    show(Some(0), Some(7), None );
    show(Some(7), Some(0), None );
    show(Some(0), Some(0), None );
    show(Some(0), None , Some(0));
    show(None , Some(0), Some(0));
    show(Some(0), Some(7), Some(0));
    show(Some(7), Some(0), Some(0));
    show(Some(0), Some(0), Some(0));
    Console.println;
  }
}


//############################################################################

object M3 {

  def test = {
    val a = new Quantity();
    val b = new Quantity();
    val c = new Quantity();
    c === (a.square + b.square).sqrt;

    a.setValue(3); b.setValue(4);
    Console.println("a = 3, b = 4 => c = " + c.str);
    a.forgetValue; b.forgetValue;

    a.setValue(3); c.setValue(5);
    Console.println("a = 3, c = 5 => b = " + b.str);
    a.forgetValue; c.forgetValue;

    b.setValue(4); c.setValue(5);
    Console.println("b = 4, c = 5 => a = " + a.str);
    b.forgetValue; c.forgetValue;

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
    ()
  }
}

//############################################################################
