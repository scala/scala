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

case class NamedSend(senderLoc: Locator, receiverLoc: Locator, data: Array[Byte], session: Symbol)

case class SendTo(a: OutputChannel[Any], msg: Any, session: Symbol)
case object Terminate

case class Locator(node: Node, name: Symbol)

/**
 * @version 0.9.17
 * @author Philipp Haller
 */
class NetKernel(service: Service) {

  def sendToNode(node: Node, msg: AnyRef) = {
    val bytes = service.serializer.serialize(msg)
    service.send(node, bytes)
  }

  def namedSend(senderLoc: Locator, receiverLoc: Locator,
                msg: AnyRef, session: Symbol) {
    val bytes = service.serializer.serialize(msg)
    sendToNode(receiverLoc.node, NamedSend(senderLoc, receiverLoc, bytes, session))
  }

  private val actors = new HashMap[Symbol, OutputChannel[Any]]
  private val names = new HashMap[OutputChannel[Any], Symbol]

  def register(name: Symbol, a: OutputChannel[Any]): Unit = synchronized {
    actors += Pair(name, a)
    names += Pair(a, name)
  }

  def getOrCreateName(from: OutputChannel[Any]) = names.get(from) match {
    case None =>
      val freshName = FreshNameCreator.newName("remotesender")
      register(freshName, from)
      freshName
    case Some(name) =>
      name
  }

  def send(node: Node, name: Symbol, msg: AnyRef): Unit =
    send(node, name, msg, 'nosession)

  def send(node: Node, name: Symbol, msg: AnyRef, session: Symbol) {
    val senderLoc = Locator(service.node, getOrCreateName(Actor.self))
    val receiverLoc = Locator(node, name)
    namedSend(senderLoc, receiverLoc, msg, session)
  }

  def forward(from: OutputChannel[Any], node: Node, name: Symbol, msg: AnyRef, session: Symbol) {
    val senderLoc = Locator(service.node, getOrCreateName(from))
    val receiverLoc = Locator(node, name)
    namedSend(senderLoc, receiverLoc, msg, session)
  }

  def createProxy(node: Node, sym: Symbol): Proxy = {
    val p = new Proxy(node, sym, this)
    proxies += Pair((node, sym), p)
    p
  }

  val proxies = new HashMap[(Node, Symbol), Proxy]

  def getOrCreateProxy(senderNode: Node, senderName: Symbol): Proxy =
    proxies.synchronized {
      proxies.get((senderNode, senderName)) match {
        case Some(senderProxy) => senderProxy
        case None              => createProxy(senderNode, senderName)
      }
    }

  /* Register proxy if no other proxy has been registered.
   */
  def registerProxy(senderNode: Node, senderName: Symbol, p: Proxy): Unit =
    proxies.synchronized {
      proxies.get((senderNode, senderName)) match {
        case Some(senderProxy) => // do nothing
        case None              => proxies += Pair((senderNode, senderName), p)
      }
    }

  def processMsg(senderNode: Node, msg: AnyRef): Unit = synchronized {
    msg match {
      case cmd@NamedSend(senderLoc, receiverLoc, data, session) =>
        Debug.info(this+": processing "+cmd)
        actors.get(receiverLoc.name) match {
          case Some(a) =>
            try {
              Debug.info(this+": receiver is "+a)
              val msg = service.serializer.deserialize(data)
              Debug.info(this+": deserialized msg is "+msg)

              val senderProxy = getOrCreateProxy(senderLoc.node, senderLoc.name)
              Debug.info(this+": created "+senderProxy)
              senderProxy.send(SendTo(a, msg, session), null)
            } catch {
              case e: Exception =>
                Debug.error(this+": caught "+e)
            }

          case None =>
            // message is lost
            Debug.info(this+": lost message")
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
