//############################################################################
// Programmation IV - 2002 - Week 03
//############################################################################
// $Id$

object M0 {
  class Rational(x: Int, y: Int) {
    def numer = x;
    def denom = y;
  }

  def addRational(r: Rational, s: Rational): Rational =
    new Rational(
      r.numer * s.denom + s.numer * r.denom,
      r.denom * s.denom);

  def makeString(r: Rational) =
    r.numer + "/" + r.denom;

  val x = new Rational(1, 2);
  val y = new Rational(1, 3);
  java.lang.System.out.println(x.numer);
  java.lang.System.out.println(x.denom);
  java.lang.System.out.println(makeString(x));
  java.lang.System.out.println(makeString(addRational(x,y)));
  java.lang.System.out.println();
}

//############################################################################

object M1 {
  class Rational(x: Int, y: Int) {
    def numer = x;
    def denom = y;
    def add(r: Rational) =
      new Rational(
        numer * r.denom + r.numer * denom,
        denom * r.denom);
    def mul(r: Rational) =
      new Rational(
        numer * r.numer,
        denom * r.denom);
    override def toString() = numer + "/" + denom;
  }

  val x = new Rational(1, 3);
  val y = new Rational(5, 7);
  val z = new Rational(3, 2);
  java.lang.System.out.println(x);
  java.lang.System.out.println(y);
  java.lang.System.out.println(z);
  java.lang.System.out.println(x.add(y).mul(z));
  java.lang.System.out.println();
}

//############################################################################

object M2 {
  class Rational(x: Int, y: Int) {
    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b);
    private val g = gcd(x, y);
    def numer = x / g;
    def denom = y / g;
    def add(r: Rational) =
      new Rational(
        numer * r.denom + r.numer * denom,
        denom * r.denom);
    def sub(r: Rational) =
      new Rational(
        numer * r.denom - r.numer * denom,
        denom * r.denom);
    def mul(r: Rational) =
      new Rational(
        numer * r.numer,
        denom * r.denom);
    def div(r: Rational) =
      new Rational(
        numer * r.denom,
        denom * r.numer);
    override def toString() = numer + "/" + denom;
  }

  val x = new Rational(1, 3);
  val y = new Rational(5, 7);
  val z = new Rational(3, 2);
  java.lang.System.out.println(x);
  java.lang.System.out.println(y);
  java.lang.System.out.println(z);
  java.lang.System.out.println(x.add(y).mul(z));
  java.lang.System.out.println();
}

//############################################################################

object M3 {
  class Rational(x: Int, y: Int) {
    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b);
    def numer = x / gcd(x, y);
    def denom = y / gcd(x, y);
    def less(that: Rational) =
      this.numer * that.denom < that.numer * this.denom;
    def max(that: Rational) = if (this.less(that)) that else this;
    override def toString() = numer + "/" + denom;
  }

  val x = new Rational(66, 42);
  val y = new Rational(42, 66);
  java.lang.System.out.println(x);
  java.lang.System.out.println(y);
  java.lang.System.out.println(x.max(y));
  java.lang.System.out.println(y.max(x));
  java.lang.System.out.println();
}

//############################################################################

object M4 {
  class Rational(x: Int, y: Int) {
    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b);
    private val g = gcd(x, y);
    def numer = x / g;
    def denom = y / g;
    def + (r: Rational) =
      new Rational(
        numer * r.denom + r.numer * denom,
        denom * r.denom);
    def - (r: Rational) =
      new Rational(
        numer * r.denom - r.numer * denom,
        denom * r.denom);
    def * (r: Rational) =
      new Rational(
        numer * r.numer,
        denom * r.denom);
    def / (r: Rational) =
      new Rational(
        numer * r.denom,
        denom * r.numer);
    override def toString() = numer + "/" + denom;
  }

  val x = new Rational(1, 2);
  val y = new Rational(1, 3);
  java.lang.System.out.println(x * x + y * y);
  java.lang.System.out.println();
}

//############################################################################

object M5 {
  trait IntSet {
    def incl(x: Int): IntSet;
    def contains(x: Int): Boolean;
  }

  class Empty extends IntSet {
    def contains(x: Int): Boolean = false;
    def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty);
  }

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    def contains(x: Int): Boolean =
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true;
    def incl(x: Int): IntSet =
      if (x < elem) new NonEmpty(elem, left incl x, right)
      else if (x > elem) new NonEmpty(elem, left, right incl x)
      else this;
  }

  val x = new Empty incl 1 incl 2;
  java.lang.System.out.println(x contains 0);
  java.lang.System.out.println(x contains 1);
  java.lang.System.out.println(x contains 2);
  java.lang.System.out.println(x contains 3);
  java.lang.System.out.println();
}

//############################################################################

object M6 {
  trait Boolean {
    def ifThenElse[a](t: => a)(e: => a): a;

    def ! : Boolean = ifThenElse[Boolean](new False())(new True());

    def && (x: => Boolean): Boolean = ifThenElse[Boolean](x)(new False());
    def || (x: => Boolean): Boolean = ifThenElse[Boolean](new True())(x);

    // !!! def == (x: Boolean): Boolean = ifThenElse[Boolean](x)(x.!);
    // !!! def != (x: Boolean): Boolean = ifThenElse[Boolean](x.!)(x);
    def <  (x: Boolean): Boolean = ifThenElse[Boolean](new False())(x);
    def >  (x: Boolean): Boolean = ifThenElse[Boolean](x.!)(new False());
    def <= (x: Boolean): Boolean = ifThenElse[Boolean](x)(new True());
    def >= (x: Boolean): Boolean = ifThenElse[Boolean](new True())(x.!);
  }
  class True() extends Boolean { // !!! class -> object
    def ifThenElse[a](t: => a)(e: => a): a = t }
  class False() extends Boolean { // !!! class -> object
    def ifThenElse[a](t: => a)(e: => a): a = e }
}

//############################################################################

object M7 {
  trait Nat {
    def isZero(): Boolean;
    def predecessor: Nat;
    def successor: Nat;
    def + (that: Nat): Nat;
    def - (that: Nat): Nat;
  }
}

//############################################################################

object M8 {

  trait IntSet {
    def incl(x: Int): IntSet;
    def contains(x: Int): Boolean;
    def map(f: Int => Int): IntSet;

    def foreach(f: Int => Unit): Unit;
    def intersect0(that: IntSet, accu: IntSet): IntSet;
    def filter0(f: Int => Boolean, accu: IntSet): IntSet;

    def intersect(that: IntSet): IntSet = intersect0(that, new Empty);
    def intersect2(that: IntSet): IntSet = filter(x => that.contains(x));
    def filter(f: Int => Boolean): IntSet = filter0(f, new Empty);

    def printOn(out: java.io.PrintStream) = foreach(out.println);

    override def toString(): String = {
      val buffer: java.lang.StringBuffer = new java.lang.StringBuffer();
      buffer.append('[');
      foreach(i => {
        if (buffer.length() > 1) {buffer.append(','); ()}; // !!! ; ()
        buffer.append(i);
        ()});
      buffer.append(']');
      buffer.toString();
    }
  }

  class Empty extends IntSet { // !!! class Empty() -> object Empty
    def contains(x: Int): Boolean = false;
    def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty);
    def map(f: Int => Int): IntSet = this;

    def foreach(f: Int => Unit): Unit = ();
    def intersect0(that: IntSet, accu: IntSet): IntSet = accu;
    def filter0(f: Int => Boolean, accu: IntSet): IntSet = accu;
  }

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    def contains(x: Int): Boolean =
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true;

    def incl(x: Int): IntSet =
      if (x < elem) new NonEmpty(elem, left incl x, right)
      else if (x > elem) new NonEmpty(elem, left, right incl x)
      else this;


    def map(f: Int => Int): IntSet = {
      val lset = left.map(f);
      val rset = right.map(f);
      new NonEmpty(f(elem), lset, rset)
    }

    def foreach(f: Int => Unit): Unit = {
      left.foreach(f);
      f(elem);
      right.foreach(f);
    }

    def intersect0(that: IntSet, accu: IntSet): IntSet =
      right.intersect0(that, left.intersect0(that,
        if (that.contains(elem)) accu.incl(elem) else accu));

    def filter0(f: Int => Boolean, accu: IntSet): IntSet =
      right.filter0(f, left.filter0(f,
        if (f(elem)) accu.incl(elem) else accu));
  }

  def test = {
    val set0: IntSet = new Empty;
    val set1: IntSet = new Empty incl 1;
    val set2: IntSet = new Empty incl 1 incl 2;
    val set3: IntSet = new Empty incl 1 incl 2 incl 3;
    val set4: IntSet = new Empty incl 1 incl 2 incl 3 incl 4;
    val setx: IntSet = set0 incl -10 incl 5 incl 21 incl -1 incl 0 incl 3;
    val sety: IntSet = set0 incl 3 incl 7 incl -5 incl 0 incl-9 incl 8 incl-1;

    java.lang.System.out.println("set0 = " + set0);
    java.lang.System.out.println("set1 = " + (set1.toString()));
    java.lang.System.out.println("set2 = " + set2);
    java.lang.System.out.println("set3 = " + (set3.toString()));
    java.lang.System.out.println("set4 = " + set4);
    java.lang.System.out.println();

    java.lang.System.out.println("set2 contains the following elements:");
    set2.foreach(java.lang.System.out.println);
    java.lang.System.out.println();

    java.lang.System.out.println("set3 contains the following elements:");
    set3 foreach java.lang.System.out.println;
    java.lang.System.out.println();

    java.lang.System.out.println("set4 contains the following elements:");
    set4.printOn(java.lang.System.out);
    java.lang.System.out.println();

    java.lang.System.out.println("2 <- set2: " + (set2 contains 2));
    java.lang.System.out.println("3 <- set2: " + set2.contains(3));
    java.lang.System.out.println();

    java.lang.System.out.println("setx     = " + setx);
    java.lang.System.out.println("setx * 2 = " + (setx.map(x => 2 * x)));
    java.lang.System.out.println();

    java.lang.System.out.println("setx        = " + setx);
    java.lang.System.out.println("sety        = " + sety);
    java.lang.System.out.println("setx & sety = " + (setx.intersect(sety)));
    java.lang.System.out.println("sety & setx = " + (sety.intersect(setx)));
    java.lang.System.out.println("setx > 0    = " + (setx.filter(x => x > 0)));
    java.lang.System.out.println("sety > 0    = " + (sety.filter(x => x > 0)));
    java.lang.System.out.println("setx & sety = " + (setx.intersect2(sety)));
    java.lang.System.out.println("sety & setx = " + (sety.intersect2(setx)));
    java.lang.System.out.println();
  }
}

//############################################################################

object M9 {
  class Rational(x: Int, y: Int) {
    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b);
    private val g = gcd(x, y);
    def numer = x / g;
    def denom = y / g;
    def add(r: Rational) =
      new Rational(
        numer * r.denom + r.numer * denom,
        denom * r.denom);
    def sub(r: Rational) =
      new Rational(
        numer * r.denom - r.numer * denom,
        denom * r.denom);
    def mul(r: Rational) =
      new Rational(
        numer * r.numer,
        denom * r.denom);
    def equal(r: Rational) =
      new Rational(
        numer * r.denom,
        denom * r.numer);
    def asString = numer.toString().concat("/").concat(denom.toString());
    override def toString() = asString;
  }

  def test = {
    java.lang.System.out.println(new Rational(2,2).asString);
    java.lang.System.out.println(new Rational(2,2).toString());
    java.lang.System.out.println(new Rational(2,2));
    java.lang.System.out.println();
  }
}

//############################################################################

object Test {
  def main(args: Array[String]): Unit = {
    M0;
    M1;
    M2;
    M3;
    M4;
    M5;
    M6;
    M7;
    M8.test;
    M9.test;
    ()
  }
}

//############################################################################
