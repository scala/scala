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
    val vs: Array[AnyVal ] = Array(6, 7l, 8f, 9d);
    val os: Array[AnyRef ] = Array("string");
    val as: Array[Any    ] = Array(0, "bye");
    println({ System.out.println("hello"); Predef}.Array());
    println(zs);
    println(bs);
    println(ss);
    println(cs);
    println(is);
    println(ls);
    println(fs);
    println(ds);
    println(vs);
    println(os);
    println(as);
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
// Main

object Test  {
  var errors: Int = 0;
  def test(bug: String, test: => Unit): Unit = {
    System.out.println("<<< bug " + bug);
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
    System.out.println(">>> bug " + bug);
    System.out.println();
  }

  def main(args: Array[String]): Unit = {

    test("Test0"  , Test0Test.main(args));
    test("Test1"  , Test1Test.main(args));

    if (errors > 0) {
      System.out.println();
      System.out.println(errors + " error" + (if (errors > 1) "s" else ""));
    }
  }
}

//############################################################################
