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

case class Send(rec: RemotePid, data: MessageTyper#DataType)
case class NamedSend(sym: Symbol, data: MessageTyper#DataType)
case class Spawn(replyto: RemotePid, p: String)
case class SpawnObject(replyto: RemotePid, data: MessageTyper#DataType)
case class Exit1(from: RemotePid, to: RemotePid, reason: Symbol)

case class RemotePidReply(res: RemotePid)
case class Disconnect()
case class NodeDown()

// CAUTION: Tells "from" to create a _uni-directional_ link!
case class Link(from: RemotePid, to: RemotePid)
case class UnLink(from: RemotePid, to: RemotePid)
case class ForwardExc(destDesc: ExcHandlerDesc, e: Throwable)

/*
case class NamedSendRep (ser:Serializer) extends TypeRep[NamedSend](ser) {
  def serialize(content: NamedSend, w: java.io.Writer): unit = {
    StringRep(ser).serialize(content.sym.name, w)
    StringRep(ser).serialize(content.data, w)
  }
  def deserialize(r:java.io.Reader): NamedSend = {
    NamedSend(Symbol(StringRep(ser).deserialize(r)),
	      StringRep(ser).deserialize(r))
  }
}
*/
