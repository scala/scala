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
  final def testFromString(x: String): Test = {
    x.charAt(0) match {
      case '*' if( x.length() == 1 ) => WildcardTest;
      case _                         => NameTest(x);
    }
  }

  case class FExp(e:Expr, c:Cond) {
    def eval(n: Node): NodeSeq = new NodeSeq { val theSeq=Nil}; // @todo
  }

  abstract class GenExp ;
  case class Attrib(test: NameTest, e: Expr) extends GenExp;

  abstract class Expr extends GenExp {
    def \ (x: String) =
      if( x=="*")
        Child(WildcardTest, this)
      else
        Child(NameTest(x), this);

    def \\ (x: String) =
      if( x=="*")
        DescOrSelf(WildcardTest, this)
      else
        DescOrSelf(NameTest(x), this);

    def apply(c: Cond) = FExp(this, c);

    def eval(n: Node): NodeSeq = new NodeSeq { val theSeq=Nil}; // @todo
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
