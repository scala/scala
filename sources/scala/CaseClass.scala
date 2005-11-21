/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;

/** defines an access function for instances of case classes
 *
 *  @author  Burak Emir
 */
trait CaseClass extends AnyRef {

  /** for a case class A(x_0,...,x_(k-1)), returns x_i for 0 &lt;= i &lt; k,
  **  null otherwise
  */
  def caseElement(n: Int): Any ;

  /** need also, for reflection
  def setCaseElement(n: Int, v: Any): unit
  */

  /** for a case class A(x_0,...,x_(k-1)), returns k
  */
  def caseArity: Int ;

  def caseName: String = ""; // for now
}
