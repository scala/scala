// $Id$

package scala.util.regexp ;

/** pointed regular hedge expressions, a useful subclass of full rhe */
trait PointedHedgeExp[ A <: Alphabet ] extends Base {

  type label = A;

  type regexp <: RegExp;

  case class  Node(label: A, r: regexp)       extends RegExp ;
  case class  TopIter(r1: regexp, r2: regexp) extends RegExp ;

  case object Point extends RegExp ;

}
