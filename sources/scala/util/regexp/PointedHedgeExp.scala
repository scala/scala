// $Id$

package scala.util.regexp ;

/** pointed regular hedge expressions, a useful subclass of
 *  regular hedge expressions.
 */
trait PointedHedgeExp extends Base {

  type T_label;
  type regexp <: RegExp;

  case class  Node(label: T_label, r: regexp) extends RegExp {
    final val isNullable = false;
  }

  case class  TopIter(r1: regexp, r2: regexp) extends RegExp {
    final val isNullable = r1.isNullable && r2.isNullable; //?
  }

  case object Point extends RegExp {
    final val isNullable = false;
  }

}
