/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors.remote

import scala.collection.mutable.{HashMap, HashSet}

case class NamedSend(senderName: Symbol, receiver: Symbol, data: Array[Byte])

class NetKernel(service: Service) {

  def sendToNode(node: Node, msg: AnyRef) = {
    val bytes = service.serializer.serialize(msg)
    service.send(node, bytes)
  }

  def namedSend(node: Node, senderName: Symbol, receiver: Symbol, msg: AnyRef): Unit = {
    val bytes = service.serializer.serialize(msg)
    sendToNode(node, NamedSend(senderName, receiver, bytes))
  }

  def send(node: Node, name: Symbol, msg: AnyRef): Unit = {
    val senderName = names.get(Actor.self) match {
      case None => {
        val freshName = FreshNameCreator.newName("remotesender")
        register(freshName, Actor.self)
        freshName
      }
      case Some(name) =>
        name
    }
    namedSend(node, senderName, name, msg)
  }

  def processMsg(senderNode: Node, msg: AnyRef): Unit = synchronized {
    msg match {
      case NamedSend(senderName, receiver, data) =>
        actors.get(receiver) match {
          case Some(a) => {
            val msg = service.serializer.deserialize(data)
            val senderProxy = new Actor {
              def act() = { a ! msg }
              override def !(msg: Any): Unit = {
                msg match {
                  case refmsg: AnyRef =>
                    namedSend(senderNode, receiver, senderName, refmsg)
                }
              }
            }
            senderProxy.start()
          }
          case None =>
            // message is lost
        }
    }
  }

  private val actors = new HashMap[Symbol, Actor]
  private val names = new HashMap[Actor, Symbol]

  /*private[actors]*/ def register(name: Symbol, a: Actor): Unit = synchronized {
    actors += name -> a
    names += a -> name
  }
}
