/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2008, LAMP/EPFL             **
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
case object Terminate

/**
 * @version 0.9.10
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
    actors += Pair(name, a)
    names += Pair(a, name)
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
    proxies += Pair((node, sym), p)
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
      case cmd@NamedSend(senderName, receiver, data) =>
        Debug.info(this+": processing "+cmd)
        actors.get(receiver) match {
          case Some(a) =>
            try {
              Debug.info(this+": receiver is "+a)
              val msg = service.serializer.deserialize(data)
              Debug.info(this+": deserialized msg is "+msg)

              val senderProxy = getOrCreateProxy(senderNode, senderName)
              Debug.info(this+": created "+senderProxy)
              senderProxy.send(SendTo(a, msg), null)
            } catch {
              case e: Exception =>
                Debug.error(this+": caught "+e)
            }

          case None =>
            // message is lost
            Debug.info(this+": lost message")
        }
      case cmd@SyncSend(senderName, receiver, data) =>
        Debug.info(this+": processing "+cmd)
        actors.get(receiver) match {
          case Some(a) =>
            val msg = service.serializer.deserialize(data)

            val senderProxy = getOrCreateProxy(senderNode, senderName)
            senderProxy.send(SyncSendTo(a, msg, receiver), null)

          case None =>
            // message is lost
        }
      case cmd@Reply(senderName, receiver, data) =>
        Debug.info(this+": processing "+cmd)
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

  def terminate() {
    // tell all proxies to terminate
    proxies.values foreach { p => p.send(Terminate, null) }

    // tell service to terminate
    service.terminate()
  }
}

class Proxy(node: Node, name: Symbol, kernel: NetKernel) extends Actor {
  start()

  override def act() {
    Debug.info(this+": waiting to process commands")
    loop {
      react {
        case cmd@SendTo(a, msg) =>
          Debug.info(this+": processing "+cmd)
          a ! msg

        case cmd@SyncSendTo(a, msg, receiver) =>
          Debug.info(this+": processing "+cmd)
          val replyCh = new Channel[Any](this)
          a.send(msg, replyCh)
          val res = replyCh.receive {
            case x => x
          }

          res match {
            case refmsg: AnyRef =>
              kernel.sendReply(node, receiver, name, refmsg)
          }

        case cmd@ReplyTo(a, msg) =>
          Debug.info(this+": processing "+cmd)
          a.replyChannel ! msg

        case cmd@Terminate =>
          Debug.info(this+": processing "+cmd)
          exit()
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
