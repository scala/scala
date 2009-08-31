/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml
package path

object Expression
{
  final def testFromString(x: String): Test = if (x == "*") WildcardTest else NameTest(x)
  private def emptyNodeSeq = new NodeSeq { val theSeq = Nil }

  case class FExp(e:Expr, c:Cond) {
    def eval(n: Node): NodeSeq = emptyNodeSeq // @todo
  }

  abstract class GenExp
  case class Attrib(test: NameTest, e: Expr) extends GenExp

  abstract class Expr extends GenExp {
    def \ (x: String)   = Child(testFromString(x), this)
    def \\ (x: String)  = DescOrSelf(testFromString(x), this)

    def apply(c: Cond) = FExp(this, c)
    def eval(n: Node): NodeSeq = emptyNodeSeq // @todo
  }

  case object Root extends Expr;

  case class Child(test: Test, e: Expr) extends Expr;
  case class DescOrSelf(test: Test, e: Expr) extends Expr;

  abstract class Test;

  case object WildcardTest extends Test; //                "x \ *  "
  case class  NameTest(label: String) extends Test; //     "x \ bar"

  abstract class Cond;

  case class Exists(p: GenExp) extends Cond ;  //          "p [ p ]"
  case class Equals(p: Expr, c:String) extends Cond ; //   "p [ @p == bla ]"
}
