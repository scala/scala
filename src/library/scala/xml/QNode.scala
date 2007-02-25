/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Node.scala 9454 2006-12-03 01:32:39 +0100 (Sun, 03 Dec 2006) emir $


package scala.xml

import compat.StringBuilder

/**
 * This object provides an extractor method to match a qualified node with its namespace URI
 *
 * @author  Burak Emir
 * @version 1.0
 */
object QNode {

  def unapplySeq(n:Node) = Some (Tuple4(n.scope.getURI(n.prefix), n.label, n.attributes, n.child))

}
