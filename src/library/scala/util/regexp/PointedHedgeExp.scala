// $Id$

package scala.util.regexp ;

/** pointed regular hedge expressions, a useful subclass of
 *  regular hedge expressions.
 */
trait PointedHedgeExp extends Base {

  type _regexpT <: RegExp;
  type _labelT;

  case class  Node(label: _labelT, r: _regexpT) extends RegExp {
    final val isNullable = false;
  }

  case class  TopIter(r1: _regexpT, r2: _regexpT) extends RegExp {
    final val isNullable = r1.isNullable && r2.isNullable; //?
  }

  case object Point extends RegExp {
    final val isNullable = false;
  }

}
