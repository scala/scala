/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors.distributed

import scala.actors.multi.ExcHandlerDesc

abstract class MessageTyper {
  type DataType = Array[Byte]
}

abstract class SystemMessage
case class Send(rec: RemotePid, data: MessageTyper#DataType) extends SystemMessage
case class NamedSend(sym: Symbol, data: MessageTyper#DataType) extends SystemMessage
case class Spawn(replyto: RemotePid, p: String) extends SystemMessage
case class SpawnObject(replyto: RemotePid, data: MessageTyper#DataType) extends SystemMessage
case class Exit1(from: RemotePid, to: RemotePid, reason: Symbol) extends SystemMessage

case class RemotePidReply(res: RemotePid) extends SystemMessage
case class Disconnect() extends SystemMessage
case class NodeDown() extends SystemMessage

// CAUTION: Tells "from" to create a _uni-directional_ link!
case class Link(from: RemotePid, to: RemotePid) extends SystemMessage
case class UnLink(from: RemotePid, to: RemotePid) extends SystemMessage
case class ForwardExc(destDesc: ExcHandlerDesc, e: Throwable) extends SystemMessage
