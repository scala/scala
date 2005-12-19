/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id:Cell.scala 5359 2005-12-16 16:33:49 +0100 (Fri, 16 Dec 2005) dubochet $
\*                                                                      */

package scala;


/** A <code>Cell</code> is a generic wrapper which completely
 *  hides the functionality of the wrapped object. The wrapped
 *  object is accessible via the <code>elem</code> accessor method.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 08/08/2003
 */
case class Cell[+T](elem: T);
