/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.math

/** <p>
 *    A trait for representing equivalence relations.  It is important to
 *    distinguish between a type that can be compared for equality or
 *    equivalence and a representation of equivalence on some type. This
 *    trait is for representing the latter.
 *  </p>
 *  <p>
 *    An <a href="http://en.wikipedia.org/wiki/Equivalence_relation">equivalence
 *    relation</a> is a binary relation on a type. This relation is exposed as
 *    the <code>equiv</code> method of the <code>Equiv</code> trait. This
 *    relation must be:
 *  </p>
 *  <ul>
 *   <li>reflexive: <code>equiv(x, x) == true</code>, for any <code>x</code> of
 *     type <code>T</code>.</li>
 *   <li>symmetric: <code>equiv(x, y) == equiv(y, x)</code>, for any
 *     <code>x</code> and <code>y</code> of type <code>T</code>.</li>
 *   <li>transitive: if <code>equiv(x, y) == true</code> and <code>equiv(y, z) == true</code>
 *     then <code>equiv(x, z) == true</code>, for any <code>x</code>, <code>y</code>,
 *     and <code>z</code> of type <code>T</code>.</li>
 *  </ul>
 *
 *  @author  Geoffrey Washburn
 *  @version 1.0, 2008-04-03
 *  @since 2.7
 */

trait Equiv[T] {
  /** Returns <code>true</code> iff <code>x</code> is equivalent to
   *  <code>y</code>.
   */
  def equiv(x: T, y: T): Boolean
}
