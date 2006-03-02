/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala;


/**
 * Function with 3 parameters
 */
trait Function3[-T0, -T1, -T2, +R] extends AnyRef {
  def apply(v0: T0, v1: T1, v2: T2): R;
  override def toString() = "<function>";
}
