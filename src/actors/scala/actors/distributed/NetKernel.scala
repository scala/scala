
package nactors.distributed

import java.io.{IOException,StringReader,StringWriter}
import java.lang.SecurityException
import java.net.UnknownHostException
import scala.collection.mutable.{HashMap,HashSet}

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
      case Some(name) => name
    }
    namedSend(node, senderName, name, msg)
  }

  def processMsg(senderNode: Node, msg: AnyRef): Unit = synchronized {
    msg match {
      case NamedSend(senderName, receiver, data) =>
        actors.get(receiver) match {
          case Some(a) => {
            Debug.info("processing message from " + senderName + " on " + senderNode)
            val msg = service.serializer.deserialize(data)
            val senderProxy = new Reactor {
              override def run() = { a ! msg }
              override def !(msg: Any): Unit = {
                msg match {
                  case refmsg: AnyRef => {
                    Debug.info("sending " + msg + " to " + senderName + " on " + senderNode)
                    namedSend(senderNode, receiver, senderName, refmsg)
                  }
                }
              }
              override def !?(msg: Any): Any =
                error("!? not implemented for remote actors.")
            }
            senderProxy.start()
          }
          case None => // message is lost
            Console.println("" + receiver + " not registered in NetKernel.")
        }
    }
  }

  private val actors = new HashMap[Symbol, Actor]
  private val names = new HashMap[Actor, Symbol]

  private[nactors] def register(name: Symbol, a: Actor): Unit = synchronized {
    Debug.info("registering " + a + " as " + name)
    actors += name -> a
    names += a -> name
  }
}
