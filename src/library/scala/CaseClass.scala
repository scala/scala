/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala


/** The trait <code>CaseClass</code> defines access functions for instances
 *  of case classes.
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
trait CaseClass extends AnyRef {

  /** for a case class <code>A(x_0,...,x_(k-1))</code>, returns <code>x_i</code>
   *  for <code>0 &lt;= i &lt; k</code>, <code>null</code> otherwise.
   *
   *  @param n the position of the n-th element
   *  @return  ...
   */
  def caseElement(n: Int): Any

  /** need also, for reflection
  def setCaseElement(n: Int, v: Any): unit
  */

  /** for a case class <code>A(x_0,...,x_(k-1))</code>, returns <code>k</code>
   */
  def caseArity: Int

  /**
   */
  def caseName: String

}
