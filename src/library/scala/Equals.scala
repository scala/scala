/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

/** An interface containing operations for equality.
 *  The only method not already present in class `AnyRef` is `canEqual`.
 */
trait Equals {

  /** A method that should be called from every well-designed equals method
   *  that is open to be overridden in a subclass. See Programming in Scala,
   *  Chapter 28 for discussion and design.
   */
  def canEqual(that: Any): Boolean

  /** The equality method defined in `AnyRef`.
   */
  def equals(that: Any): Boolean
}
