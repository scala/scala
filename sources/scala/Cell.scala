/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
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
