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

case class NamedSend(senderName: Symbol, receiver: Symbol, data: Array[Byte])
case class SyncSend(senderName: Symbol, receiver: Symbol, data: Array[Byte])
case class Reply(senderName: Symbol, receiver: Symbol, data: Array[Byte])

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

  def processMsg(senderNode: Node, msg: AnyRef): Unit = synchronized {
    msg match {
      case NamedSend(senderName, receiver, data) =>
        actors.get(receiver) match {
          case Some(a) =>
            val msg = service.serializer.deserialize(data)
            val senderProxy = new Actor {
              def act() = { a ! msg }
              override def !(msg: Any) {
                msg match {
                  case refmsg: AnyRef =>
                    // node, senderName, receiver, msg
                    namedSend(senderNode, receiver, senderName, refmsg)
                }
              }
            }
            senderProxy.start(); {}
          case None =>
            // message is lost
        }
      case SyncSend(senderName, receiver, data) =>
        actors.get(receiver) match {
          case Some(a) =>
            val msg = service.serializer.deserialize(data)
            val senderProxy = new Actor {
              def act() = {
                val res = a !? msg
                res match {
                  case refmsg: AnyRef =>
                    // node, senderName, receiver, msg
                    sendReply(senderNode, receiver, senderName, refmsg)
                }
              }
            }
            senderProxy.start(); {}
          case None =>
            // message is lost
        }
      case Reply(senderName, receiver, data) =>
        actors.get(receiver) match {
          case Some(a) =>
            val msg = service.serializer.deserialize(data)
            val senderProxy = new Actor {
              def act() = {
                a.replyChannel ! msg
              }
            }
            senderProxy.start(); {}
          case None =>
            // message is lost
        }
    }
  }
}
