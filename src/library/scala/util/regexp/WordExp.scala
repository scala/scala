/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.util.regexp

/**
 *  The class `WordExp` provides regular word expressions.
 *
 *  Users have to instantiate type member `_regexpT <;: RegExp` (from class `Base`)
 *  and a type member `_labelT <;: Label`.
 *
 *  Here is a short example:
 *  <pre>
 *  <b>import</b> scala.util.regexp._
 *  <b>import</b> scala.util.automata._
 *  <b>object</b> MyLang <b>extends</b> WordExp {
 *    <b>type</b> _regexpT = RegExp
 *    <b>type</b> _labelT = MyChar
 *
 *    <b>case class</b> MyChar(c:Char) <b>extends</b> Label
 *  }
 *  <b>import</b> MyLang._
 *  // (a* | b)*
 *  <b>val</b> rex = Star(Alt(Star(Letter(MyChar('a'))),Letter(MyChar('b'))))
 *  <b>object</b> MyBerriSethi <b>extends</b> WordBerrySethi {
 *    <b>override val</b> lang = MyLang
 *  }
 *  <b>val</b> nfa = MyBerriSethi.automatonFrom(Sequ(rex), 1)
 *  </pre>
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
abstract class WordExp extends Base {

  abstract class Label

  type _regexpT <: RegExp
  type _labelT <: Label

  case class Letter(a: _labelT) extends RegExp {
    final lazy val isNullable = false
    var pos = -1
  }

  case class Wildcard() extends RegExp {
    final lazy val isNullable = false
    var pos = -1
  }
}

