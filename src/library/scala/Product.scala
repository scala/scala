/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

/** The trait <code>Product</code> defines access functions for instances
 *  of products, in particular case classes.
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
trait Product extends AnyRef {

  /** for a case class <code>A(x_1,...,x_k))</code>, returns <code>x_(i+1)</code>
   *  for <code>0 &lt;= i &lt; k</code>
   *
   *  @param  n the position of the n-th element
   *  @throws IndexOutOfBoundsException
   *  @return  ...
   */
  def productElement(n: Int): Any

  /** return k for a product <code>A(x_1,...,x_k))</code>
   */
  def productArity: Int

  /**
   *  By default the empty string. Implementations may override this
   *  method in order to prepend a string prefix to the result of the
   *  toString methods.
   */
  def productPrefix = ""

}
