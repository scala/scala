/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors.distributed

/**
 * @author Philipp Haller
 */
abstract class JXTAServiceBase(nodename: String) extends Thread with Service {
  val serializer = new JavaSerializer(this)
  private val internalNode = new JXTANode(nodename)
  def node: Node = internalNode
  def createPid(actor: RemoteActor): RemotePid =
    new JXTAPid(internalNode, makeUid, kernel, actor)
}
