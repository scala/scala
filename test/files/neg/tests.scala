//############################################################################
// Compile Time Errors
//############################################################################
// $Id$

import java.lang.System; // to avoid name clash with .NET's library

//############################################################################
// Test 0 - Block Qualifiers

package test0.bar {

  object System {
    val out: PrintStream = new PrintStream();
  }

  class PrintStream() {
    def println(): Unit = {
      java.lang.System.out.println();
    }
  }

}

object Test0Test {

  def main(args: Array[String]): Unit = {
    {System.out.print(10)}; java.lang.System.out.println();
    {System.out.print(11); java}.lang.System.out.println();
    {System.out.print(12); java.lang}.System.out.println();
    {System.out.print(13); java.lang.System}.out.println();
    {System.out.print(14); java.lang.System.out}.println();
    {System.out.print(15); java.lang.System.out.println:(() => Unit)}();
    {System.out.print(16); java.lang.System.out.println()};

    {System.out.print(20)}; test0.bar.System.out.println();
    {System.out.print(21); test0}.bar.System.out.println();
    {System.out.print(22); test0.bar}.System.out.println();
    {System.out.print(23); test0.bar.System}.out.println();
    {System.out.print(24); test0.bar.System.out}.println();
    {System.out.print(25); test0.bar.System.out.println:(() => Unit)}();
    {System.out.print(26); test0.bar.System.out.println()};
  }

}

//############################################################################
// Test 1 - References to Generated Classes

trait Test1 {
 def a: Any = new scala.Tuple2$class(1,1);
 def b: scala.Predef$$anon$7;
 def c: scala.List$Class;
}

//############################################################################
