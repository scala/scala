/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala

/** A trait for representing equivalence relations.
 *  @author  Geoffrey Washburn
 *  @version 0.9, 2008-03-19
 */

trait Equiv[T] {
  /** Returns <code>true</code> iff <code>x</code> is equivalent to
  /** <code>y</code>.  The implementation
  /*  should be equivalence relation: reflexive, transitive,
  /*  symmetric.
   */
  def equiv(x: T, y: T): Boolean
}
