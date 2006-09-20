/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util.regexp

/** regular word expressions. users have to instantiate type member
 *  _regexpT &lt;: RegExp (from Base) and a type member _labelT &lt;: Label
 *  Here is a little example:
 *  <pre>
  import scala.util.regexp._
  import scala.util.automata._
  object MyLang extends WordExp {
    type _regexpT = RegExp
    type _labelT = MyChar

    case class MyChar(c:Char) extends Label
  }
  import MyLang._
  // (a* | b)*
  val rex = Star(Alt(Star(Letter(MyChar('a'))),Letter(MyChar('b'))))
  object MyBerriSethi extends WordBerrySethi {
    override val lang = MyLang
  }
  val nfa = MyBerriSethi.automatonFrom(Sequ(rex),1)
 *  </pre>
 */
abstract class WordExp extends Base {

  abstract class Label

  type _regexpT <: RegExp
  type _labelT <: Label

  case class Letter(a: _labelT) extends RegExp {
    final val isNullable = false
    var pos = -1
  }

  case class Wildcard() extends RegExp {
    final val isNullable = false
    var pos = -1
  }
}

