/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors.remote

import scala.collection.mutable.{HashMap, HashSet}
import scala.actors.Actor.loop

case class NamedSend(senderName: Symbol, receiver: Symbol, data: Array[Byte])
case class SyncSend(senderName: Symbol, receiver: Symbol, data: Array[Byte])
case class Reply(senderName: Symbol, receiver: Symbol, data: Array[Byte])

case class SendTo(a: Actor, msg: Any)
case class SyncSendTo(a: Actor, msg: Any, receiver: Symbol)
case class ReplyTo(a: Actor, msg: Any)

/**
 * @version 0.9.8
 * @author Philipp Haller
 */
class NetKernel(service: Service) {

  def sendToNode(node: Node, msg: AnyRef) = {
    val bytes = service.serializer.serialize(msg)
    service.send(node, bytes)
  }

  def namedSend(node: Node, sender: Symbol, receiver: Symbol, msg: AnyRef) {
    val bytes = service.serializer.serialize(msg)
    sendToNode(node, NamedSend(sender, receiver, bytes))
  }

  def namedSyncSend(node: Node, sender: Symbol, receiver: Symbol, msg: AnyRef) {
    val bytes = service.serializer.serialize(msg)
    val toSend = SyncSend(sender, receiver, bytes)
    sendToNode(node, toSend)
  }

  def sendReply(node: Node, sender: Symbol, receiver: Symbol, msg: AnyRef) {
    val bytes = service.serializer.serialize(msg)
    val toSend = Reply(sender, receiver, bytes)
    sendToNode(node, toSend)
  }

  private val actors = new HashMap[Symbol, Actor]
  private val names = new HashMap[Actor, Symbol]

  def register(name: Symbol, a: Actor): Unit = synchronized {
    actors += name -> a
    names += a -> name
  }

  def selfName = names.get(Actor.self) match {
    case None =>
      val freshName = FreshNameCreator.newName("remotesender")
      register(freshName, Actor.self)
      freshName
    case Some(name) =>
      name
  }

  def send(node: Node, name: Symbol, msg: AnyRef) {
    val senderName = selfName
    namedSend(node, senderName, name, msg)
  }

  def syncSend(node: Node, name: Symbol, msg: AnyRef) {
    val senderName = selfName
    namedSyncSend(node, senderName, name, msg)
  }

  def createProxy(node: Node, sym: Symbol): Actor = {
    val p = new Proxy(node, sym, this)
    proxies += Pair(node, sym) -> p
    p
  }

  val proxies = new HashMap[(Node, Symbol), Actor]

  def getOrCreateProxy(senderNode: Node, senderName: Symbol): Actor = synchronized {
    proxies.get((senderNode, senderName)) match {
      case Some(senderProxy) => senderProxy
      case None => createProxy(senderNode, senderName)
    }
  }

  def processMsg(senderNode: Node, msg: AnyRef): Unit = synchronized {
    msg match {
      case NamedSend(senderName, receiver, data) =>
        actors.get(receiver) match {
          case Some(a) =>
            val msg = service.serializer.deserialize(data)

            val senderProxy = getOrCreateProxy(senderNode, senderName)
            senderProxy.send(SendTo(a, msg), null)

          case None =>
            // message is lost
        }
      case SyncSend(senderName, receiver, data) =>
        actors.get(receiver) match {
          case Some(a) =>
            val msg = service.serializer.deserialize(data)

            val senderProxy = getOrCreateProxy(senderNode, senderName)
            senderProxy.send(SyncSendTo(a, msg, receiver), null)

          case None =>
            // message is lost
        }
      case Reply(senderName, receiver, data) =>
        actors.get(receiver) match {
          case Some(a) =>
            val msg = service.serializer.deserialize(data)

            val senderProxy = getOrCreateProxy(senderNode, senderName)
            senderProxy.send(ReplyTo(a, msg), null)

          case None =>
            // message is lost
        }
    }
  }
}

class Proxy(node: Node, name: Symbol, kernel: NetKernel) extends Actor {
  start()

  override def act() {
    loop {
      react {
        case SendTo(a, msg) =>
          a ! msg

        case SyncSendTo(a, msg, receiver) =>
          val replyCh = new Channel[Any](this)
          a.send(msg, replyCh)
          val res = replyCh.receive {
            case x => x
          }

          res match {
            case refmsg: AnyRef =>
              kernel.sendReply(node, receiver, name, refmsg)
          }

        case ReplyTo(a, msg) =>
          a.replyChannel ! msg
      }
    }
  }

  override def !(msg: Any): Unit = msg match {
    case ch ! m =>
      // do not send remotely
      this.send(msg, Actor.self.replyChannel)
    case a: AnyRef =>
      kernel.send(node, name, a)
    case other =>
      error("Cannot send non-AnyRef value remotely.")
  }

  override def !?(msg: Any): Any = msg match {
    case a: AnyRef =>
      val replyCh = Actor.self.freshReplyChannel
      kernel.syncSend(node, name, a)
      replyCh.receive {
        case x => x
      }
    case other =>
      error("Cannot send non-AnyRef value remotely.")
  }
}
