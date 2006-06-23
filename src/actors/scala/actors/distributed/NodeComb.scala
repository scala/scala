/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors.distributed

import scala.actors.distributed.picklers.BytePickle._

/**
 * @author Philipp Haller
 */
object NodeComb {
  def tcpNodePU: SPU[TcpNode] =
    wrap((p: Pair[String,int]) => TcpNode(p._1, p._2),
         (n: TcpNode) => Pair(n.address, n.port), pair(string, nat));
  def jxtaNodePU: SPU[JXTANode] =
    wrap((s: String) => JXTANode(s),
         (n: JXTANode) => n.name, string);
}
