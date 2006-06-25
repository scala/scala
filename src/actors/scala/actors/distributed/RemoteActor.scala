/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors.distributed

import scala.actors.multi.{MailBox,Actor,Pid,LocalPid,ExcHandlerDesc}
import scala.collection.mutable.{HashMap,Stack}

abstract class ServiceName
case class JXTA(groupName: String) extends ServiceName
case class TCP() extends ServiceName

/**
 * @author Philipp Haller
 */
class RemoteActor extends Actor {

  override def forwardExc(destDesc: ExcHandlerDesc, e: Throwable) = {
    // locality check (handler local to this actor?)
    if (destDesc.pid == self)
      handleExc(destDesc, e)
    else
      kernel.forwardExc(destDesc, e)
  }

  override def receive(f: PartialFunction[Message,Unit]): scala.All = {
    if (isAlive) {
      continuation = null
      sent.dequeueFirst(f.isDefinedAt) match {
        case Some(msg) =>
	  try {
            f(msg)
          }
          catch {
            case d: Done =>
              throw new Done
            case t: Throwable =>
              if (!excHandlerDescs.isEmpty)
                forwardExc(excHandlerDescs.top, t)
              else
                die(Symbol(t.toString()))
          }
          die()
        case None =>
          continuation = f
          Debug.info("No msg found. " + this + " has continuation " + continuation + ".")
      }
    }
    throw new Done
  }

  var kernel: NetKernel = null

  def node = self.node

  def nodes = kernel.nodes

  private var selfCached: RemotePid = null

  override def self: RemotePid = {
    if (selfCached == null)
      selfCached = kernel pidOf this
    selfCached
  }

  def serialize(index: String, rep: Serializer => AnyRef) =
    kernel.registerSerializer(index, rep)

  def alive(s: ServiceName): Unit = {
    var service: Service = null
    s match {
      case TCP() =>
        val serv = new TcpService(TcpService.generatePort)
        service = serv
        serv.start()
      /*case JXTA(groupName) =>
        val serv = new ch.epfl.lamp.scala.actors.jxta.JXTAService("AliveActor"
				    + new java.util.Date().getTime() + "-"
				    + new java.util.Random().nextInt(1000),
				    java.util.logging.Level.FINEST) {
          //millis before we give up group creation and create the group.
          override def TIME_BEFORE_AUTO_GROUP_CREATE: long = 30000
	  val PIPE_ID:String="1"
          //val ADV_LIFETIME:long = 1 * 60 * 60 * 1000 //millis to keep advertisements ...
          override def  MY_GROUP_NAME:String = groupName
          /*val SENDER_MESSAGE = "PalcomDemo"; //used to identify the message element in jxta messages
          val PIPE_BASE_ID:String = "PIPE4Pal4"+MY_GROUP_NAME;
          val MESSAGE_THRESHOLD = 5;*/
        }
        service = serv
        serv.start()*/
      case _ =>
        throw new Exception ("Unknown Service in RemoteActor")
    }
    // create RemotePid
    selfCached = service.kernel.register(this)
  }

  def node(pid: Pid): Node = pid match {
    case rpid: RemotePid => rpid.node
    case lpid: LocalPid => null
  }

  def disconnectNode(node: Node) =
    kernel.disconnectNode(node)

  //does not call start def of Actor
  def register(name: Symbol, pid: RemotePid): Unit =
    kernel.registerName(name, pid)

  //calls start def of Actor
  def register(name: Symbol, a: RemoteActor): Unit =
    kernel.registerName(name, a)

  def name(node: Node, sym: Symbol): Name =
    Name(node, sym, kernel)

  def spawn(node: Node, name: String): RemotePid =
    kernel.spawn(self, node, name)

  def spawn(node: Node, a: RemoteActor): Unit =
    kernel.spawn(self, node, a)

  def spawn(fun: RemoteActor => Unit): RemotePid =
    kernel.spawn(fun)

  def spawn(a: RemoteActor): RemotePid = {
    val pid = kernel.register(a)
    a.start
    pid
  }

  def spawnLink(fun: RemoteActor => unit): RemotePid =
    kernel.spawnLink(self, fun)

  def monitorNode(node: Node, cond: Boolean) =
    kernel.monitorNode(self, node, cond)

  // this should be:
  // self.link(pid)
  // if self is RemotePid it will invoke NetKernel

  def link(pid: RemotePid): Unit =
    kernel.link(self, pid)

  def unlink(pid: RemotePid): Unit =
    kernel.unlink(self, pid)

  override def exit(reason: Symbol): Unit =
    kernel.exit(self, reason)

  override def processFlag(flag: Symbol, set: Boolean) =
    kernel.processFlag(self, flag, set)

  override def die(reason: Symbol) =
    if (isAlive) {
      isAlive = false
      Debug.info("" + this + " died.")
      kernel.exit(self, reason)
    }

  override def die() =
    if (isAlive) {
      isAlive = false
      Debug.info("" + this + " died.")
      kernel.exit(self, 'normal)
    }

}
