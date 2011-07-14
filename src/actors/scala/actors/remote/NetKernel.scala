/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.actors
package remote

import scala.collection.mutable

case class NamedSend(senderLoc: Locator, receiverLoc: Locator, data: Array[Byte], session: Symbol)

case class RemoteApply0(senderLoc: Locator, receiverLoc: Locator, rfun: Function2[AbstractActor, Proxy, Unit])
case class LocalApply0(rfun: Function2[AbstractActor, Proxy, Unit], a: AbstractActor)

case class  SendTo(a: OutputChannel[Any], msg: Any, session: Symbol)
case object Terminate

case class Locator(node: Node, name: Symbol)

/**
 * @version 0.9.17
 * @author Philipp Haller
 */
private[remote] class NetKernel(service: Service) {

  def sendToNode(node: Node, msg: AnyRef) = {
    val bytes = service.serializer.serialize(msg)
    service.send(node, bytes)
  }

  def namedSend(senderLoc: Locator, receiverLoc: Locator,
                msg: AnyRef, session: Symbol) {
    val bytes = service.serializer.serialize(msg)
    sendToNode(receiverLoc.node, NamedSend(senderLoc, receiverLoc, bytes, session))
  }

  private val actors = new mutable.HashMap[Symbol, OutputChannel[Any]]
  private val names = new mutable.HashMap[OutputChannel[Any], Symbol]

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

  def remoteApply(node: Node, name: Symbol, from: OutputChannel[Any], rfun: Function2[AbstractActor, Proxy, Unit]) {
    val senderLoc = Locator(service.node, getOrCreateName(from))
    val receiverLoc = Locator(node, name)
    sendToNode(receiverLoc.node, RemoteApply0(senderLoc, receiverLoc, rfun))
  }

  def createProxy(node: Node, sym: Symbol): Proxy = {
    val p = new Proxy(node, sym, this)
    proxies += Pair((node, sym), p)
    p
  }

  val proxies = new mutable.HashMap[(Node, Symbol), Proxy]

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
      case cmd@RemoteApply0(senderLoc, receiverLoc, rfun) =>
        Debug.info(this+": processing "+cmd)
        actors.get(receiverLoc.name) match {
          case Some(a) =>
            val senderProxy = getOrCreateProxy(senderLoc.node, senderLoc.name)
            senderProxy.send(LocalApply0(rfun, a.asInstanceOf[AbstractActor]), null)

          case None =>
            // message is lost
            Debug.info(this+": lost message")
        }

      case cmd@NamedSend(senderLoc, receiverLoc, data, session) =>
        Debug.info(this+": processing "+cmd)
        actors.get(receiverLoc.name) match {
          case Some(a) =>
            try {
              val msg = service.serializer.deserialize(data)
              val senderProxy = getOrCreateProxy(senderLoc.node, senderLoc.name)
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
    proxies.values foreach { _.send(Terminate, null) }

    // tell service to terminate
    service.terminate()
  }
}
