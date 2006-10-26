//############################################################################
// Test Java interaction with scala inner classes
//############################################################################
// $Id:  $

import java.io.{BufferedReader, File, FileWriter, InputStreamReader}

/** The entry point of this test. It has to come first,
 *  before the package declarations. The parser wouldn't want it
 *  any other way.
 */
object Test {
  def main(args: Array[String]): Unit = {
    val b = new p.b.B;
    val c = new b.C;
    c.m

    val ji = new p.b.JavaInteraction(Array('a', 'b', 'c'));
    (new ji.Inner).m;

    (new p.b.OuterObj.Inner).m
  }
}

package p {
  package a {

    class A {
      protected val x = 10;

      protected def meth1(x: Int) = x + 1;
      protected def meth2(x: Int)(y: Int) = x + y;
      protected def meth3 = Array(1, 2)

      def getA: this.type = this;
    }
  }

  package b {
    import a._;

    /** Test interraction with Scala inherited methods and currying. */
    class B extends A {
      class C {
        def m = {
          Console.println(x);
          Console.println("meth1(1) = " + meth1(1));
          // test accesses from closures
          for (val x <- 1 until 3)
            Console.println("meth2(1)(1) = " + meth2(1)(1));

          Console.println("meth3 = " + meth3.getClass);

          val inc = &meth2(1);
          Console.println("10++ = " + inc(10));

          getA.x;
        }
      }
    }

    /** Test interaction with Java inherited protected fields. */
    class JavaInteraction(arr: Array[Char]) extends java.io.CharArrayReader(arr) {
      class Inner {
        def m = {
          Console.println("count before: " + count);
          count = count + 1;
          Console.println("count after: " + count);
        }
      }
    }

    /** Test interaction when outer is an object. */
    object OuterObj extends p.a.A {
      class Inner {
        def m = {
          Console.println(x);
          Console.println("meth1(1) = " + meth1(1));
          Console.println("meth2(1)(1) = " + meth2(1)(1));

          val inc = &meth2(1);
          Console.println("10++ = " + inc(10));

          getA.x;
        }
      }
    }
  }
}
