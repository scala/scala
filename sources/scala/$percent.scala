/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;


/** An xml element
 *
 *  @author  Burak Emir
 *  @version 1.0, 26.11.2003
 */
abstract case class %(s:Symbol,ns:Seq[scala.xml.Node])
   extends scala.xml.nobinding.Element(s,ns) { }

