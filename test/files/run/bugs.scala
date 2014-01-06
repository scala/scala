//############################################################################
// Bugs
//############################################################################

//############################################################################
// Bug 98

object Bug98Test {
  object MyCase { def name = "mycase" }
  def test(args: Array[String]) {
    println(MyCase.name)
  }
}

//############################################################################
// Bug 120

class Bug120A(x: Int) {
  println("A")
}

trait Bug120B {
  println("B")
}
class Bug120C(x: Int)
    extends Bug120A(Bug120Test.print("one", 1))
    with Bug120B {
  println("C")
}
object Bug120Test {
  def print[A](str: String, res: A): A = {
    println(str); res
  }
  def test(args: Array[String]) {
    val c = new Bug120C(1)
    ()
  }
}

//############################################################################
// Bug 135

object Bug135Test {

  import scala.collection.immutable.TreeMap

  def test(args: Array[String]) {
    val myMap:TreeMap[Int, String] = new TreeMap
    val map1 = myMap + ((42, "The answer"))
    println(map1.get(42))
  }

}

//############################################################################
// Bug 142

abstract class Bug142Foo1 { class Inner; def foo: Inner; foo; }
abstract class Bug142Foo2 { class Inner; def foo: Inner = {Console.println("ok"); null};}
abstract class Bug142Foo3 { type  Inner; def foo: Inner; foo; }
abstract class Bug142Foo4 { type  Inner; def foo: Inner = {Console.println("ok"); null.asInstanceOf[Inner]}; }

trait Bug142Bar1 { type  Inner; def foo: Inner = {Console.println("ok"); null.asInstanceOf[Inner]}; }
trait Bug142Bar2 { type  Inner; def foo: Inner; foo; }
trait Bug142Bar3 { class Inner; def foo: Inner = {Console.println("ok"); null}; }
trait Bug142Bar4 { class Inner; def foo: Inner; foo; }

object Bug142Test1 extends Bug142Foo1 with Bug142Bar1 { def test(args: Array[String]) {} }
object Bug142Test2 extends Bug142Foo2 with Bug142Bar2 { def test(args: Array[String]) {} }
object Bug142Test3 extends Bug142Foo3 with Bug142Bar3 { def test(args: Array[String]) {} }
object Bug142Test4 extends Bug142Foo4 with Bug142Bar4 { def test(args: Array[String]) {} }
object Bug142Test5 extends Bug142Foo1 with Bug142Bar1 { def test(args: Array[String]) {} }
object Bug142Test6 extends Bug142Foo2 with Bug142Bar2 { def test(args: Array[String]) {} }
object Bug142Test7 extends Bug142Foo3 with Bug142Bar3 { def test(args: Array[String]) {} }
object Bug142Test8 extends Bug142Foo4 with Bug142Bar4 { def test(args: Array[String]) {} }

object Bug142Test {
  def test(args:Array[String]) {
    Bug142Test1;
    Bug142Test2;
    Bug142Test3;
    Bug142Test4;
    Bug142Test5;
    Bug142Test6;
    Bug142Test7;
    Bug142Test8;
    ()
  }
}

//############################################################################
// Bug 166

object Bug166Test {
  import scala.collection.mutable.HashMap
  def test(args: Array[String]) {
    val m: HashMap[String,String] = new HashMap[String, String]
    m.update("foo","bar")
  }
}

//############################################################################
// Bug 167

class Bug167Node(bar:Int) {
  val foo = {
    val bar = 1;
    bar
  }
}

object Bug167Test {
  def test(args: Array[String]) {
    if (new Bug167Node(0).foo != 1) println("bug 167");
  }
}

//############################################################################
// Bug 168

class Bug168Foo {
  class Bar
  def foo = new Bar
}

object Bug168Test {
  def test(args: Array[String]) {
    (new Bug168Foo).foo
    ()
  }
}

//############################################################################
// Bug 174

class Bug174Foo[X] {

  class Tree
  class Node extends Tree


  val inner: Inner = new SubInner

  trait Inner {
    def test: Bug174Foo[X]#Tree
  }

  class SubInner extends Inner {
    def test = new Node
  }

}

object Bug174Test {
  def test(args: Array[String]) {
    (new Bug174Foo[Int]).inner.test
    ()
  }
}


//############################################################################
// Bug 176

trait Bug176A {
  type T;
  def foo(x: T): Int;
  def bar: T;
  def test = foo(bar);
}
trait Bug176B {
  type S <: AnyRef;
  type T = S;
  def foo(x: S): Int;
  def bar: S;
}
class Bug176C extends Bug176A with Bug176B {
  class S;
  def foo(x: S) = 1;
  def bar = new S;
}
object Bug176Test {
  def test(args: Array[String]): Unit = {
    val x: Bug176A = new Bug176C;
    Console.println(x.test);
  }
}

//############################################################################
// Bug 199

class Bug199C { object o; }
object Bug199Test {
  def test(args: Array[String]) = {
    (new Bug199C).o; ()
  }
}

//############################################################################
// Bug 213

trait Bug213Foo {
  def testAll: Unit;
  def testAllRef: String;
}

class Bug213Bar extends Bug213Foo {
  def testAll = (().asInstanceOf[Nothing] : Nothing);
  def testAllRef = ("".asInstanceOf[Null] : Null);
}

object Bug213Test {
  def test(args: Array[String]): Unit = {
    val foo: Bug213Foo = new Bug213Bar;
    try {
      foo.testAll;
    } catch {
      case e: ClassCastException =>
        Console.println("Cannot cast unit to Nothing");
    }
    try {
      foo.testAllRef;
    } catch {
      case e: ClassCastException =>
        Console.println("Cannot cast empty string to Null");
    }
    ()
  }
}

//############################################################################
// Bug 217

object Bug217Test {
  def foo[t](fun: Function0[t]): t = fun();
  def bar(x: Int): Unit = {
    foo(() => 0);
    ()
  }
  def test(args: Array[String]): Unit = bar(32);
}

//############################################################################
// Bug 222

object Bug222Test {
  def test(args:Array[String]): Unit = {
    val array: Array[String] = new Array(16);
    ()
  }
}

//############################################################################
// Bug 225

case class Bug225C();

object Bug225Test {

  def test(args: Array[String]): Unit = {
    val a = new Array[Array[Bug225C]](2);
    a(0) = new Array[Bug225C](2);
    a(0)(0) = new Bug225C();
  }
}

//############################################################################
// Bug 226

object Bug226Test {

  def id[a](xs: Array[a]): Array[a] = xs;

  def test(args: Array[String]) {
    var xs = new Array[Int](1);
    class X { xs };
    xs = id(xs);
    id(xs);
    ()
  }

}

//############################################################################
// Bug 233

object Bug233Test {
  val b: Array[String] = null;
  def test(args: Array[String]): Unit =
    Console.println(b == null);
}

//############################################################################
// Bug 250

object Bug250Test {
  def test(args: Array[String]) {
    if (true) null;
    ()
  }
}

//############################################################################
// Bug 257

object Bug257Test {
  def sayhello(): Unit = { Console.println("I should come 1st and 2nd"); };
  def sayhi(): Unit = { Console.println("I should come last"); };

  def f1(x: Unit): Unit = ();
  def f2(x: Unit)(y: Unit): Unit = ();

  def f(x: => Unit): Unit => Unit = {
    f1(x);
    f2(x);
  }

  def test(args: Array[String]) {
    f(sayhello())(sayhi())
  }
}

//############################################################################
// Bug 266

// version - A

abstract class Bug266AFoo {
  type T >: Null <: AnyRef;
  abstract class I0 { def f(x: T): Unit; f(null); }
}

object Bug266ATest extends Bug266AFoo {
  type T = String;
  class I1 extends I0 { def f(x: String) { Console.println("hello") } }
  def test(args: Array[String]): Unit = { new I1; () }
}

// version - B

abstract class Bug266BA {
  type t
  abstract class P {
    def f(x: t): Unit
  }
}

abstract class Bug266BA1 extends Bug266BA {
  def mkP: Bug266BA1.this.P;
  val in: t;
}

trait Bug266BB extends Bug266BA {
  type t = Int;
  class P1 extends Bug266BB.this.P {
    def f(x: Int) { Console.println(x + 1) }
  }
  def mkP = new P1;
  val in = 3;
}

object Bug266BTest {
  val a: Bug266BA1 = new Bug266BA1 with Bug266BB;
  def test(args: Array[String]): Unit = a.mkP.f(a.in);
}

// main

object Bug266Test {
  def test(args: Array[String]) {
    Bug266ATest.test(args);
    Bug266BTest.test(args);
  }
}

//############################################################################
// Bug 316

class Bug316MyIterator extends Iterator[Int] {
  def hasNext = false
  def next = 42
}

object Bug316Test {
  def test(args: Array[String]): Unit =
    (new Bug316MyIterator) filter { x: Int => x == 1 };
}

//############################################################################
// Bug 328

object Bug328Test {
  def test0(f: Function1[Int,String]) {}
  def test(args: Array[String]): Unit = test0(args);
}

//############################################################################
// Bug 396

trait Bug396A {
  class I {
    def run = Console.println("A");
  }
}
trait Bug396B extends Bug396A {
  class I extends super.I {
    override def run = { super.run; Console.println("B"); }
  }
}
trait Bug396C extends Bug396A {
  trait I extends super.I {
    override def run = { super.run; Console.println("C"); }
  }
}
object Bug396Test extends Bug396B with Bug396C {
  class I2 extends super[Bug396B].I with super[Bug396C].I;
  def test(args: Array[String]): Unit = (new I2).run
}

//############################################################################
// Bug 399

object Bug399Test {
  def f(x: String): String = {
    trait C { def f: String = x; }
    class D extends C;
    trait F extends C;
    class G extends D with F;
    (new G).f
  }

  def test(args: Array[String]) {
    Console.println(f("a"));
  }
}

//############################################################################
// Main

object Test  {
  var errors: Int = 0
  def test(bug: Int, test: => Unit) {
    Console.println("<<< bug " + bug)
    try {
      test;
    } catch {
      case exception: Throwable =>
        Console.print("Exception in thread \"" + Thread.currentThread + "\" " + exception);
        Console.println;
        errors += 1
    }
    Console.println(">>> bug " + bug)
    Console.println
  }

  def main(args: Array[String]) {

    test( 98, Bug98Test.test(args));
    test(120, Bug120Test.test(args));
    test(135, Bug135Test.test(args));
    test(142, Bug142Test.test(args));
    test(166, Bug166Test.test(args));
    test(167, Bug167Test.test(args));
    test(168, Bug168Test.test(args));
    test(174, Bug174Test.test(args));
    test(176, Bug176Test.test(args));
    test(199, Bug199Test.test(args));
    test(213, Bug213Test.test(args));
    test(217, Bug217Test.test(args));
    test(222, Bug222Test.test(args));
    test(225, Bug225Test.test(args));
    test(226, Bug226Test.test(args));
    test(233, Bug233Test.test(args));
    test(250, Bug250Test.test(args));
    test(257, Bug257Test.test(args));
    test(266, Bug266Test.test(args));
    test(316, Bug316Test.test(args));
    test(328, Bug328Test.test(args));
    test(396, Bug396Test.test(args));
    test(399, Bug399Test.test(args));

    if (errors > 0) {
      Console.println;
      Console.println(errors + " error" + (if (errors > 1) "s" else ""));
    }
  }
}

//############################################################################
