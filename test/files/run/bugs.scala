//############################################################################
// Bugs
//############################################################################
// $Id$

import java.lang.System; // to avoid name clash with .NET's library

//############################################################################
// Bug 98

object Bug98Test {
  object MyCase { def name = "mycase" };
  def main(args: Array[String]) = {
    System.out.println(MyCase.name);
  }
}

//############################################################################
// Bug 120

class Bug120A(x: Int) {
  System.out.println("A");
}
class Bug120B(x: Int) {
  System.out.println("B");
}
class Bug120C(x: Int)
    with Bug120A(Bug120Test.print("one", 1))
    with Bug120B(Bug120Test.print("two", 2)) {
  System.out.println("C");
}
object Bug120Test {
  def print[A](str: String, res: A): A = {
    System.out.println(str); res
  }
  def main(args: Array[String]) = {
    val c = new Bug120C(1);
  }
}

//############################################################################
// Bug 135

object Bug135Test {

  import scala.collection.immutable.TreeMap;
  import scala.collection.immutable.Order;

  def main(args: Array[String]): Unit = {
    val intOrder =
	new Order((x:int,y:int) => x < y, (x:int,y:int) => x == y);
    val myMap:TreeMap[int,String] = new TreeMap(intOrder);
    val map1 = myMap + 42 -> "The answer";
    Console.println(map1.get(42));
  }

}

//############################################################################
// Bug 142

abstract class Bug142Foo1 { class Inner; def foo: Inner; foo; }
abstract class Bug142Foo2 { class Inner; def foo: Inner = {System.out.println("ok"); null};}
abstract class Bug142Foo3 { type  Inner; def foo: Inner; foo; }
abstract class Bug142Foo4 { type  Inner; def foo: Inner = {System.out.println("ok"); null.asInstanceOf[Inner]}; }

abstract class Bug142Bar1 { type  Inner; def foo: Inner = {System.out.println("ok"); null.asInstanceOf[Inner]}; }
abstract class Bug142Bar2 { type  Inner; def foo: Inner; foo; }
abstract class Bug142Bar3 { class Inner; def foo: Inner = {System.out.println("ok"); null}; }
abstract class Bug142Bar4 { class Inner; def foo: Inner; foo; }

object Bug142Test1 extends Bug142Foo1 with Bug142Bar1 {def main(args:Array[String]):Unit=();}
object Bug142Test2 extends Bug142Foo2 with Bug142Bar2 {def main(args:Array[String]):Unit=();}
object Bug142Test3 extends Bug142Foo3 with Bug142Bar3 {def main(args:Array[String]):Unit=();}
object Bug142Test4 extends Bug142Foo4 with Bug142Bar4 {def main(args:Array[String]):Unit=();}
object Bug142Test5 with    Bug142Foo1 with Bug142Bar1 {def main(args:Array[String]):Unit=();}
object Bug142Test6 with    Bug142Foo2 with Bug142Bar2 {def main(args:Array[String]):Unit=();}
object Bug142Test7 with    Bug142Foo3 with Bug142Bar3 {def main(args:Array[String]):Unit=();}
object Bug142Test8 with    Bug142Foo4 with Bug142Bar4 {def main(args:Array[String]):Unit=();}

object Bug142Test {
  def main(args:Array[String]): Unit = {
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
  import scala.collection.mutable.HashMap ;
  def main(args:Array[String]) = {
    val m:HashMap[String,String] = new HashMap[String,String];
    m.update("foo","bar");
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
  def main(args: Array[String]): Unit = {
    if (new Bug167Node(0).foo != 1) System.out.println("bug 167");
  }
}

//############################################################################
// Bug 168

class Bug168Foo {
  class Bar;
  def foo = new Bar;
}

object Bug168Test {
  def main(args: Array[String]): Unit = {
    (new Bug168Foo).foo;
    ()
  }
}

//############################################################################
// Bug 174

class Bug174Foo[X] {

  class Tree;
  class Node extends Tree;


  val inner:Inner = new SubInner;

  trait Inner {
    def test: Bug174Foo[X]#Tree ;
  }

  class SubInner extends Inner {
    def test = new Node;
  }

}

object Bug174Test {
  def main(args: Array[String]): Unit = {
    (new Bug174Foo[Int]).inner.test;
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
  type S <: Object;
  type T = S;
  def foo(x: S): Int;
  def bar: S;
}
class Bug176C with Bug176A with Bug176B {
  class S;
  def foo(x: S) = 1;
  def bar = new S;
}
object Bug176Test {
  def main(args: Array[String]): Unit = {
    val x: Bug176A = new Bug176C;
    System.out.println(x.test);
  }
}

//############################################################################
// Bug 199

class Bug199C { object o; }
object Bug199Test {
  def main(args: Array[String]) = {
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
  def testAll = (().asInstanceOf[All] : All);
  def testAllRef = ("".asInstanceOf[AllRef] : AllRef);
}

object Bug213Test {
  def main(args: Array[String]): Unit = {
    val foo: Bug213Foo = new Bug213Bar;
    foo.testAll;
    foo.testAllRef;
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
  def main(args: Array[String]): Unit = bar(32);
}

//############################################################################
// Bug 222

object Bug222Test {
  def main(args:Array[String]): Unit = {
    val array: Array[String] = new Array(16);
    ()
  }
}

//############################################################################
// Bug 225

case class Bug225C();

object Bug225Test {

  def main(args: Array[String]): Unit = {
    val a = new Array[Array[Bug225C]](2);
    a(0) = new Array[Bug225C](2);
    a(0)(0) = new Bug225C();
  }
}

//############################################################################
// Bug 226

object Bug226Test {

  def id[a](xs: Array[a]): Array[a] = xs;

  def main(args: Array[String]): Unit = {
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
  def main(args: Array[String]): Unit =
    System.out.println(b == null);
}

//############################################################################
// Bug 250

object Bug250Test {
  def main(args: Array[String]): Unit = {
    if (true) null;
    ()
  }
}

//############################################################################
// Main

object Test  {
  var errors: Int = 0;
  def test(bug: Int, def test: Unit): Unit = {
    System.out.println("<<< bug " + bug);
    try {
      test;
    } catch {
      case exception => {
        val name: String = Thread.currentThread().getName();
        System.out.print("Exception in thread \"" + name + "\" ");
        exception.printStackTrace();
        System.out.println();
        errors = errors + 1;
      }
    }
    System.out.println(">>> bug " + bug);
    System.out.println();
  }

  def main(args: Array[String]): Unit = {

    test( 98, Bug98Test.main(args));
    test(120, Bug120Test.main(args));
    test(135, Bug135Test.main(args));
    test(142, Bug142Test.main(args));
    test(166, Bug166Test.main(args));
    test(167, Bug167Test.main(args));
    test(168, Bug168Test.main(args));
    test(174, Bug174Test.main(args));
    test(176, Bug176Test.main(args));
    test(199, Bug199Test.main(args));
    test(213, Bug213Test.main(args));
    test(217, Bug217Test.main(args));
    test(222, Bug222Test.main(args));
    test(225, Bug225Test.main(args));
    test(226, Bug226Test.main(args));
    test(233, Bug233Test.main(args));
    test(250, Bug250Test.main(args));

    if (errors > 0) {
      System.out.println();
      System.out.println(errors + " error" + (if (errors > 1) "s" else ""));
    }
  }
}

//############################################################################
