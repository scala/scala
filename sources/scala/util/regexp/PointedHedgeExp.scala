// $Id$

package scala.util.regexp ;

import scala.util.alphabet.Alphabet ;

/** pointed regular hedge expressions, a useful subclass of
 *  regular hedge expressions.
 */
trait PointedHedgeExp[ A <: Alphabet ] extends Base {

  type label = A;

  type regexp <: RegExp;

  case class  Node(label: A, r: regexp)       extends RegExp {
    final val isNullable = false;
  }
  case class  TopIter(r1: regexp, r2: regexp) extends RegExp {
    final val isNullable = r1.isNullable && r2.isNullable; //?
  }

  case object Point extends RegExp {
    final val isNullable = false;
  }

}
