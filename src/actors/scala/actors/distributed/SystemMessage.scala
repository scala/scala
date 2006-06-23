/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors.distributed

import scala.actors.multi.Pid
import scala.actors.multi.ExcHandlerDesc

abstract class MessageTyper {
  type DataType = Array[Byte]
}

abstract class SystemMessage
case class Send(rec: Pid, data: MessageTyper#DataType) extends SystemMessage
case class Spawn(replyto: Pid, p: String) extends SystemMessage
case class PidReply(res: Pid) extends SystemMessage
case class Disconnect() extends SystemMessage

case class NodeDown() extends SystemMessage

// CAUTION: Tells "from" to create a _uni-directional_ link!
case class Link(from: Pid, to: Pid) extends SystemMessage
case class UnLink(from: Pid, to: Pid) extends SystemMessage
case class Exit1(from: Pid, to: Pid, reason: Symbol) extends SystemMessage

case class SpawnObject(replyto: Pid, data: MessageTyper#DataType) extends SystemMessage

case class NamedSend(sym: Symbol, data: MessageTyper#DataType) extends SystemMessage

case class ForwardExc(destDesc: ExcHandlerDesc, e: Throwable) extends SystemMessage

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
