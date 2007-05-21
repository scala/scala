/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors.remote

/**
 *  @author Philipp Haller
 */
trait Service {
  val kernel = new NetKernel(this)
  val serializer: Serializer
  def node: Node
  def send(node: Node, data: Array[Byte]): Unit
}
