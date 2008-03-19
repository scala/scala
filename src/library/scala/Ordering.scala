/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala

/** A trait for representing total orderings.
 *  @author  Geoffrey Washburn
 *  @version 0.9, 2008-03-19
 */

trait Ordering[T] extends Equiv[T] {
  def compare(x: T, y: T): Int
  def gteq(x: T, y: T): Boolean = compare(x, y) >= 0
  def lteq(x: T, y: T): Boolean = compare(x, y) <= 0
  def lt(x: T, y: T): Boolean = compare(x, y) < 0
  def gt(x: T, y: T): Boolean = compare(x, y) > 0
}
