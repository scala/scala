/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2003, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;

/** defines an access function for instances of case classes (not case objects)
 *
 *  @author  Burak Emir
 */
trait CaseClass {

  /** for a case class A(x1,...,xk), returns x_i for 1<= i <= k, null otherwise
  */
  def selectElement(n:int):Any ;

  /** for a case class A(x1,...,xk), returns k
  */
  def numberOfElements(): int ;

}
