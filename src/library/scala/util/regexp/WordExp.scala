// $Id$

package scala.util.regexp ;

/** regular word expressions.
 */
trait WordExp extends Base {

  trait Label;

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

