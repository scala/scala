// $Id$

module Test {

def fac(n: Int): Int = if (n < 2) 1 else fac(n - 1) * n;

// Fibonacci


def fib(n: Int): Int = if (n < 2) 1 else fib(n - 1) + fib(n - 2);

def show_fib(n: Int): Int = {
    java.lang.System.out.print("### fib(");
    java.lang.System.out.print(n);
    java.lang.System.out.print(") = ");
    java.lang.System.out.flush();
    val v = fib(n);
    java.lang.System.out.print(v);
    java.lang.System.out.println();
    java.lang.System.out.flush();
    v
}

def id[X](x: X): X = x;

def apply[X](f: X => X, x: X): X = f(x);

def id_obj(x: java.lang.Object): java.lang.Object = x;

def apply_obj(f: java.lang.Object => java.lang.Object, x: java.lang.Object): java.lang.Object = f(x);

def id_any(x: scala.Any): scala.Any = x;

def apply_any(f: scala.Any => scala.Any, x: scala.Any): scala.Any = f(x);

def id_int(x: Int): Int = x;

def apply_int(f: Int => Int, x: Int): Int = f(x);

class MyClass() {
    override def toString() = "=== MyClass::toString ===";
    def test() = java.lang.System.out.println("=== MyClass::test ===");
}

class MySubclass() extends MyClass() {
    override def toString() = "=== MySubclass::toString ===";
}

def foobar = {
    42;
    42l;
    23.5f;
    23.5;
    "Hello";
    32 + 45;
    // !!! System
    // java; // !!! why is this legal ? what  does it return ?
    // java.lang;
    // java.lang.System; // !!! return the Class object ?
    java.lang.System.out;
    java.lang.System.out.println("### Hello");
    java.lang.System.out.print("### ");
    java.lang.System.out.println(17);
    // !!! java.lang.System.out.println("### " + 13);
    java.lang.System.out.println("### Bye");
    java.lang.System.out.println();
    val x = 13;
    x;
    // !!! why are DefDef replaced by Block(Tree[0])? we should use Empty!
    def f = 19;
    f;
    def f0() = 11;
    f0();
    def f1(x: Int) = x;
    f1(7);
    def f2(x: Int, y: Int) = x + y;
    f2(3,5);
    def f11(x: Int)(y: Int) = x + y;
    f11(23)(2);
    1 < 2;
    if (1 < 2) 3 else 4;


    show_fib(0);
    show_fib(1);
    show_fib(2);
    show_fib(3);
    show_fib(4);

    // !!! show_fib(id[Int](4));

/*
    show_fib(5);
    show_fib(6);
    show_fib(7);
    show_fib(8);
    show_fib(9);
    show_fib(10);
    show_fib(11);
    show_fib(12);
*/

    val map: java.util.Map = new java.util.HashMap();
    // why do we insert type casts as[scala.Any]();
    map.put("Hello", new java.lang.Integer(32));
    java.lang.System.out.println("Hello -> " + map.get("Hello"));
    // !!! apply(map.get, "Hello");
    java.lang.System.out.println();

    val myObj = new MyClass();
    java.lang.System.out.println(myObj);
    val mySub = new MySubclass();
    java.lang.System.out.println(mySub);
    // java.lang.System.out.println(myObj.test()); // !!! strange type error
    myObj.test();
    java.lang.System.out.println();

    System.out.println(apply_any(id_any, "identity").toString());
    //apply[java.lang.String](id[java.lang.String], "identity")
    System.out.println();
};

foobar;

//############################################################################

class A(a: Int) {
  def getA = a;
}

class B(b: Int, c: Int) extends A(b + c) {
  def getB = b;
}

class X(x: Int) {
  def getX = x;
}
case class Y(y: Int, z: Int) extends X(y + z) {
  def getY = y;
  def getAA = this.y;
}

{
  val a: A = new A(1);
  val b: B = new B(2,3);

  val x: X = new X(4);
  val y: Y = new Y(5,6);

  System.out.println("A.a = " + a.getA);
  System.out.println("B.a = " + b.getA);
  System.out.println("B.b = " + b.getB);
  System.out.println();

  System.out.println("X.a = " + x.getX);
  System.out.println("Y.a = " + y.getX);
  System.out.println("Y.b = " + y.getY);
  System.out.println("Y.b = " + y.y);
  System.out.println();
}

//############################################################################

{
class X() {

  def foo = {
    System.out.println("X::foo");
  }

}

class Y() extends X() {

  override def foo = {
    System.out.println("Y::foo");
    super.foo;
  }

}

val x: X = new X();
val y: X = new Y();

x.foo;
System.out.println();

y.foo;
System.out.println();
}

//############################################################################

{
class X() {}

class O(a: Int) {


  case class Y(b: Int) extends X() {
    override def toString() = "";
    def bar = a + b;
  }

  def foo = Y(2).bar
}

System.out.println(new O(1).foo)
}

{

class O(a: Int) {

  class X() {}

  case class Y(b: Int) extends X() {
    override def toString() = "";
    def bar = a + b;
  }

  def foo = Y(2).bar
}

System.out.println(new O(1).foo)
}

System.out.println();

//############################################################################

  def main(args: Array[String]): Unit = {
    ()
  }

//############################################################################
}
