/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;

trait Ord[t <: Ord[t]]: t {
  def < (that: t): Boolean;
  def <=(that: t): Boolean = this < that || this == that;
  def > (that: t): Boolean = that < this;
  def >=(that: t): Boolean = that <= this;
}

/* Shall we use a covariant Ord?

trait Ord[+T <: Ord[T]] {
  def < [S >: T <: Ord[S]](that: S): Boolean;
  def <=[S >: T <: Ord[S]](that: S): Boolean = this < that || this == that;
  def > [S >: T <: Ord[S]](that: S): Boolean = that < this;
  def >=[S >: T <: Ord[S]](that: S): Boolean = that <= this;
}
*/
