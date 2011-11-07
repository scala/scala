//############################################################################
// Test Java interaction with scala inner classes
//############################################################################

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
      protected def meth1(x: Double) = x + 1
      protected def meth2(x: Int)(y: String) = y + (x - 1);
      protected def meth3 = Array(1, 2)

      protected def f[a](x: a) = x

      def getA: this.type = this;
    }
    
    /** Test type members */
    trait HighlighterXXX {
      type Node;
      protected def highlight(node : Node) : Unit;
    }
    
    /** Test type parameters */
    abstract class PolyA[a] {
      protected def m(x: a): Unit;

      class B {

        trait Node {
          def s: String = "";
        }
        protected def tie(x: Node): Unit = { x.s; () }
      }
    }

    /** bug 853, longer path members */
    class Global {
      abstract class Tree;
    }

    trait HasNSC {
      trait GlobalExt extends Global;
      val global : GlobalExt;
      import global._;
      protected def doTyped(tree : Tree): Tree = tree;
      def mkTree : Tree;
      doTyped(mkTree);
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
          Console.println("meth1(1.0) = " + meth1(1.0));
          // test accesses from closures
          for (val x <- 1 until 3)
            Console.println("meth2(1)(1) = " + meth2(1)("prefix: "));

          Console.println("meth3 = " + meth3.getClass);

          val inc = meth2(1)_;
          Console.println("100 = " + inc("10"));

          Console.println("id(1) = " + f(1))
          Console.println("id('a') = " + f("a"))

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
          Console.println("meth2(1)(1) = " + meth2(1)("1"));

          val inc = meth2(1)_;
          Console.println("100 = " + inc("10"));
          
          getA.x;
        }
      }
    }
    
    trait ScalaAutoEditXXX extends HighlighterXXX {
      trait NodeImpl { 
        def self : Node;
        highlight(self);
      }
    }
    
    abstract class X[T] extends PolyA[T] {

      trait Inner extends B { 
        def self: T;
        def self2: Node;
        def getB: Inner;

        m(self)

        trait InnerInner {
          val g = getB
          g.tie(self2.asInstanceOf[g.Node])
        }
      }
    }

    trait ScalaTyperXXX extends HasNSC {
      val global : GlobalExt;
      import global._;
      trait XXX {
        def foo(tree : Tree) = doTyped(tree);
      }
    }
  }
}
