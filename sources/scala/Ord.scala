/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;

/* Shall we use a nonvariant Ord?

trait Ord[t <: Ord[t]]: t {
  def < (that: t): Boolean;
  def <=(that: t): Boolean = this < that || this == that;
  def > (that: t): Boolean = that < this;
  def >=(that: t): Boolean = that <= this;
}

*/

trait Ord[+T <: Ord[T]]: T {
  def < [S >: T <: Ord[S]](that: S): Boolean;
  def <=[S >: T <: Ord[S]](that: S): Boolean = this < that || this == that;
  def > [S >: T <: Ord[S]](that: S): Boolean = that < this;
  def >=[S >: T <: Ord[S]](that: S): Boolean = that <= this;
  def min[S >: T <: Ord[S]](that: S): S = if (this < that) this else that;
  def max[S >: T <: Ord[S]](that: S): S = if (this < that) that else this;
}

/*
trait Ord[+a] {
  def compareTo [b >: a <% Ord[b]](that: b): int;
  def <  [b >: a <% Ord[b]](that: b): boolean = (this compareTo that) <  0;
  def >  [b >: a <% Ord[b]](that: b): boolean = (this compareTo that) >  0;
  def <= [b >: a <% Ord[b]](that: b): boolean = (this compareTo that) <= 0;
  def >= [b >: a <% Ord[b]](that: b): boolean = (this compareTo that) >= 0;
}
*/
