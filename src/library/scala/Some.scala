/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2003, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id:Some.scala 5359 2005-12-16 16:33:49 +0100 (Fri, 16 Dec 2005) dubochet $
\*                                                                      */

package scala;


/** Class <code>Option[A]</code> represents existing values of type
 *  <code>A</code>.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 16/07/2003
 */
final case class Some[+A1](x: A1) extends Option[A1];
