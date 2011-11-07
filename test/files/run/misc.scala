object Test {

  def fac(n: Int): Int = if (n < 2) 1 else fac(n - 1) * n;

  // Fibonacci
  def fib(n: Int): Int = if (n < 2) 1 else fib(n - 1) + fib(n - 2);

  def show_fib(n: Int): Int = {
    Console.print("### fib(");
    Console.print(n);
    Console.print(") = ");
    Console.flush;
    val v = fib(n);
    Console.print(v);
    Console.println;
    Console.flush;
    v
  }

  def id[X](x: X): X = x;

  def apply[X](f: X => X, x: X): X = f(x);

  def id_obj(x: AnyRef): AnyRef = x;

  def apply_obj(f: AnyRef => AnyRef, x: AnyRef): AnyRef = f(x);

  def id_any(x: scala.Any): scala.Any = x;

  def apply_any(f: scala.Any => scala.Any, x: scala.Any): scala.Any = f(x);

  def id_int(x: Int): Int = x;

  def apply_int(f: Int => Int, x: Int): Int = f(x);

  class MyClass() {
    override def toString() = "=== MyClass::toString ===";
    def test() = Console.println("=== MyClass::test ===");
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
    //System.out;
    Console.println("### Hello");
    Console.print("### ");
    Console.println(17);
    Console.println("### Bye");
    Console.println;
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

    val myObj = new MyClass();
    Console.println(myObj);
    val mySub = new MySubclass();
    Console.println(mySub);
    myObj.test();
    Console.println;

    Console.println(apply_any(id_any, "identity").toString());
    Console.println;
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

  Console.println("A.a = " + a.getA);
  Console.println("B.a = " + b.getA);
  Console.println("B.b = " + b.getB);
  Console.println;

  Console.println("X.a = " + x.getX);
  Console.println("Y.a = " + y.getX);
  Console.println("Y.b = " + y.getY);
  Console.println("Y.b = " + y.y);
  Console.println;
}

//############################################################################

{
class X() {

  def foo = {
    Console.println("X::foo");
  }

}

class Y() extends X() {

  override def foo = {
    Console.println("Y::foo");
    super.foo;
  }

}

val x: X = new X();
val y: X = new Y();

x.foo;
Console.println;

y.foo;
Console.println;
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

Console.println(new O(1).foo)
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

Console.println(new O(1).foo)
}

Console.println;

  case class Bar();

  case class Foo(i: Int, j: Char, c: Bar) ;

  Console.println(
    true // Foo(3,'a',Bar()).caseElement( -1 ) == null // throws Exception now
    && Foo(3,'a',Bar()).productElement( 0 ) == 3
    && Foo(3,'a',Bar()).productElement( 1 ) == 'a'
    && Foo(3,'a',Bar()).productElement( 2 ) == Bar()
    && true // Foo(3,'a',Bar()).caseElement( 3 ) == null // throws Exception now
    && Bar().productArity == 0
    && Foo(3,'a',Bar()).productArity == 3);

//############################################################################

  def main(args: Array[String]) {
  }

//############################################################################
}
