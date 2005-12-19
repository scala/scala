/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id:Symbol.scala 5359 2005-12-16 16:33:49 +0100 (Fri, 16 Dec 2005) dubochet $
\*                                                                      */

package scala;


/** Instances of <code>Symbol</code> can be created easily with
 *  Scala's built-in quote mechanism. For instance, the Scala term
 *  <code>'mysym</code> will invoke the constructor of the
 *  <code>Symbol</code> class in the following way:
 *  <code>new Symbol("mysym")</code>. .
 *
 *  @author  Martin Odersky
 *  @version 1.7, 08/12/2003
 */
final case class Symbol(name: String) {

  /** Converts this symbol to a string.
   */
  override def toString(): String = {
    "'" + name
  }

}
