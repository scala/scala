/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala;


/** A partial function of type <code>PartialFunction[A, B]</code> is a
 *  unary function where the domain does not include all values of type
 *  <code>A</code>. The function <code>isDefinedAt</code> allows to
 *  test dynamically, if a value is in the domain of the function.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 16/07/2003
 */
trait PartialFunction[-A, +B] extends AnyRef with (A => B) {

  /** Checks if a value is contained in the functions domain.
   *
   *  @param  x   the value to test
   *  @return true, iff <code>x</code> is in the domain of this function.
   */
  def isDefinedAt(x: A): Boolean;

  def orElse[A1 <: A, B1 >: B](that: PartialFunction[A1, B1]) = new PartialFunction[A1, B1] {
    def isDefinedAt(x: A1): Boolean =
      PartialFunction.this.isDefinedAt(x) || that.isDefinedAt(x)
    def apply(x: A1): B1 =
      if (PartialFunction.this.isDefinedAt(x)) PartialFunction.this.apply(x)
      else that.apply(x)
  }

  override def andThen[C](k: B => C) = new PartialFunction[A, C] {
    def isDefinedAt(x: A): Boolean = PartialFunction.this.isDefinedAt(x)
    def apply(x: A): C = k(PartialFunction.this.apply(x))
  }
}

