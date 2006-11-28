
/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// generated on Tue Nov 28 14:03:53 CET 2006
// $Id$

package scala


/**
 * Function with 3 parameters.
 */
trait Function3 [-T1, -T2, -T3, +R] extends AnyRef {
  def apply(v1:T1, v2:T2, v3:T3): R
  override def toString() = "<function>"

}
