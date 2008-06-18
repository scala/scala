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
 * @version 0.9.10
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

  def createProxy(node: Node, sym: Symbol): Actor = {
    val p = Proxy(node, sym, this)
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

case class Proxy(node: Node, name: Symbol, kernel: NetKernel) extends Actor {
  start()

  @transient
  private var channelMap = new HashMap[Symbol, OutputChannel[Any]]

  @transient
  private var sessionMap = new HashMap[Channel[Any], Symbol]

  def act() {
    Debug.info(this+": waiting to process commands")
    loop {
      react {
        // Request from remote proxy.
        // `this` is local proxy.
        case cmd@SendTo(out, msg, session) =>
          Debug.info(this+": processing "+cmd)

          // is this an active session?
          channelMap.get(session) match {
            case None =>
              // create a new reply channel...
              val replyCh = new Channel[Any](this)

              // ...that maps to session
              sessionMap += Pair(replyCh, session)

              // local send to actor
              val a = out.asInstanceOf[Actor]
              a.send(msg, replyCh)

            case Some(replyCh) =>
              replyCh ! msg
              // TODO:
              // remove `replyCh` from mapping
              // to avoid memory leak (always safe?)
              // or: use WeakHashMap
              // however, it's the value (channel)
              // that should be weak!
          }

        case cmd@Terminate =>
          Debug.info(this+": processing "+cmd)
          exit()

        // local proxy receives response to
        // reply channel
        case ch ! resp =>
          // lookup session ID
          sessionMap.get(ch) match {
            case Some(sid) =>
              val msg = resp.asInstanceOf[AnyRef]
              // send back response
              kernel.forward(sender, node, name, msg, sid)

            case None =>
              Debug.info(this+": cannot find session for "+ch)
          }

        // remote proxy receives request
        case msg: AnyRef =>
          // create fresh session ID...
          val sid = FreshNameCreator.newName(node+"@"+name)

          // ...that maps to reply channel
          channelMap += Pair(sid, sender)

          kernel.forward(sender, node, name, msg, sid)
      }
    }
  }

}
