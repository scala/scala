//############################################################################
// Run Time Bugs & Test Cases
//############################################################################
// $Id$

import java.lang.System; // to avoid name clash with .NET's library

//############################################################################
// serves as an entry point with the MSIL backend

object TestMain {
  def main(args: Array[String]): Unit = {
    Test.main(args);
  }
}

//############################################################################
// Test 0 - Array creation

object Test0Test {
  def println[A](xs: Array[A]): Unit = {
    var i = 0;
    System.out.print("[");
    while (i < xs.length) {
      if (i > 0) System.out.print(",");
      System.out.print(xs(i));
      i = i + 1;
    }
    System.out.print("]");
    System.out.println();
  }

  def main(args: Array[String]): Unit = {
    val zs: Array[Boolean] = Array(false, true);
    val bs: Array[Byte   ] = Array(0, 1, 2);
    val ss: Array[Short  ] = Array(3, 4, 5);
    val cs: Array[Char   ] = Array('a', 'b', 'c');
    val is: Array[Int    ] = Array(6, 7, 8);
    val ls: Array[Long   ] = Array(9l, 10l, 11l);
    val fs: Array[Float  ] = Array(12.0f, 13.0f);
    val ds: Array[Double ] = Array(14.0d, 15.0d);
    val os: Array[AnyRef ] = Array("string");
    println(zs);
    println(bs);
    println(ss);
    println(cs);
    println(is);
    println(ls);
    println(fs);
    println(ds);
    println(os);
  }
}

//############################################################################
// Test 1 - Block Qualifiers

package test1.bar {

  object System {
    val out: PrintStream = new PrintStream();
  }

  class PrintStream() {
    def println(): Unit = {
      java.lang.System.out.println();
    }
  }

}

object Test1Test {

  def main(args: Array[String]): Unit = {
    {System.out.print(10)}; java.lang.System.out.println();
    // {System.out.print(11); java}.lang.System.out.println();
    // {System.out.print(12); java.lang}.System.out.println();
    // {System.out.print(13); java.lang.System}.out.println();
    {System.out.print(14); java.lang.System.out}.println();
    {System.out.print(15); java.lang.System.out.println:(() => Unit)}();
    {System.out.print(16); java.lang.System.out.println()};

    {System.out.print(20)}; test1.bar.System.out.println();
    // {System.out.print(21); test1}.bar.System.out.println();
    // {System.out.print(22); test1.bar}.System.out.println();
    {System.out.print(23); test1.bar.System}.out.println();
    {System.out.print(24); test1.bar.System.out}.println();
    {System.out.print(25); test1.bar.System.out.println:(() => Unit)}();
    {System.out.print(26); test1.bar.System.out.println()};
  }

}

//############################################################################
// Test 2 - Super Calls with Mixins

package test2 {

  class A {
    def run = System.out.println("A");
  }

  class M0 extends A {
    override def run = { super.run; System.out.println("M0"); }
  }

  class M1 extends M0 {
    override def run = { super.run; System.out.println("M1"); }
  }

  class N0 extends A {
    override def run = { super.run; System.out.println("N0"); }
  }

  class N1 extends N0 {
    override def run = { super.run; System.out.println("N1"); }
  }

  object M0N0 extends M0 with N0;
  object N0M0 extends N0 with M0;
  object M1N0 extends M1 with N0;
  object N1M0 extends N1 with M0;

}

object Test2Test {
  def main(args: Array[String]): Unit = {
    test2.M0N0.run; System.out.println();
    test2.N0M0.run; System.out.println();
    test2.M1N0.run; System.out.println();
    test2.N1M0.run; System.out.println();
  }
}

//############################################################################
// Main

object Test  {
  var errors: Int = 0;
  def test(name: String, test: => Unit): Unit = {
    System.out.println("<<< " + name);
    try {
      test;
    } catch {
      case exception => {
        val name: String = Thread.currentThread().getName();
        System.out.print("Exception in thread \"" + name + "\" " + exception);
        System.out.println();
        errors = errors + 1;
      }
    }
    System.out.println(">>> " + name);
    System.out.println();
  }

  def main(args: Array[String]): Unit = {

    test("Test0"  , Test0Test.main(args));
    test("Test1"  , Test1Test.main(args));
    test("Test2"  , Test2Test.main(args));

    if (errors > 0) {
      System.out.println();
      System.out.println(errors + " error" + (if (errors > 1) "s" else ""));
    }
  }
}

//############################################################################
