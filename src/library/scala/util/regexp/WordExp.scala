/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util.regexp ;


/** regular word expressions.
 */
abstract class WordExp extends Base {

  abstract class Label;

  type _regexpT <: RegExp ;
  type _labelT <: Label;

  case class Letter(a: _labelT) extends RegExp {
    final val isNullable = false;
    var pos = -1;
  }

  case class Wildcard() extends RegExp {
    final val isNullable = false;
    var pos = -1;
  }
}

