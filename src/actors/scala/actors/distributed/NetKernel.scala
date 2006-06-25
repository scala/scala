/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors.distributed

import java.io.{IOException,StringReader,StringWriter}
import java.lang.SecurityException
import java.net.UnknownHostException
import java.util.logging.{ConsoleHandler,Level,Logger}

import scala.collection.mutable.{HashMap,HashSet}
import scala.actors.multi.{Actor,ExcHandlerDesc}

case class RA(a: RemoteActor)

object NetKernel {
  var kernel: NetKernel = null
}

/**
 * @author Philipp Haller
 */
class NetKernel(service: Service) {
  NetKernel.kernel = this

  // contains constructors
  private val ptable =
    new HashMap[String, () => RemoteActor]

  // maps local ids to scala.actors
  private val rtable =
    new HashMap[int, RemoteActor]

  // maps scala.actors to their RemotePid
  private val pidTable =
    new HashMap[RemoteActor, RemotePid]

  private var running = true;

  val logLevel = Level.FINE
  val l = Logger.getLogger("NetKernel")
  l.setLevel(logLevel)
  val consHand = new ConsoleHandler
  consHand.setLevel(logLevel)
  l.addHandler(consHand)

  //start // start NetKernel

  /** only called if destDesc is local. */
  def handleExc(destDesc: ExcHandlerDesc, e: Throwable) =
    destDesc.pid match {
      case rpid: RemotePid =>
        (rtable get rpid.localId) match {
          case Some(actor) =>
            actor.handleExc(destDesc, e)
          case None =>
            error("exc desc refers to non-registered actor")
        }
    }

  def forwardExc(destDesc: ExcHandlerDesc, e: Throwable) =
    // locality check (handler local to this node?)
    destDesc.pid match {
      case rpid: RemotePid =>
        if (rpid.node == this.node)
          handleExc(destDesc, e)
        else
          sendToNode(rpid.node, ForwardExc(destDesc, e))
    }

  def sendToNode(node: Node, msg: AnyRef) = {
    //val sw = new StringWriter
    val bytes = service.serializer.serialize(msg /*, sw*/)
    service.send(node, bytes)
    //service.send(node, sw.toString())
  }

  def addConstructor(key: String, value: () => RemoteActor) =
    ptable.update(key, value);

  def node: Node = service.node

  def nodes: List[Node] = service.nodes

  def pidOf(actor: RemoteActor): RemotePid = synchronized {
    pidTable.get(actor) match {
      case None => error("malformed pid table in " + this)
      case Some(pid) => pid
    }
  }

  def disconnectNode(n: Node) = synchronized {
    service.disconnectNode(n)
  }

  def getLocalRef(locId: Int): RemoteActor =
    rtable.get(locId) match {
      case None =>
        error("" + locId + " is not registered at " + this)
      case Some(remoteActor: RemoteActor) =>
        remoteActor
    }

  def localSend(localId: Int, msg: AnyRef): Unit = synchronized {
    rtable.get(localId) match {
      case None =>
        error("" + localId + " is not registered at " + this)
      case Some(remoteActor: RemoteActor) =>
        //Console.println("local send to " + remoteActor)
        remoteActor send msg
    }
  }

  def localSend(pid: RemotePid, msg: AnyRef): Unit =
    localSend(pid.localId, msg)

  def remoteSend(pid: RemotePid, msg: AnyRef) = synchronized {
    //Console.println("NetKernel: Remote msg delivery to " + pid)
    service.remoteSend(pid, msg)
  }

  def namedSend(name: Name, msg: AnyRef): Unit =
    if (name.node == this.node) {
      // look-up name
      nameTable.get(name.sym) match {
        case None =>
          // message is lost
          //Console.println("lost message " + msg + " because " + name + " not registered.")
        case Some(localId) => localSend(localId, msg)
      }
    }
    else {
      // remote send

      // serialize msg
      ///val sw = new StringWriter
      val bytes = service.serializer.serialize(msg/*, sw*/)

      sendToNode(name.node, NamedSend(name.sym, bytes))
      //sendToNode(name.node, NamedSend(name.sym, sw.toString()))
    }

  val nameTable = new HashMap[Symbol, Int]

  def registerName(name: Symbol, pid: RemotePid): Unit = synchronized {
    nameTable += name -> pid.localId
  }

  def registerName(name: Symbol, a: RemoteActor): Unit = synchronized {
    val pid = register(a)
    registerName(name, pid)
    a.start
  }

  /*override def run: unit = receive {
    case ForwardExc(destDesc, e) =>
      // TODO
    case Spawn(reply: RemotePid, pname) =>
      val newPid = spawn(pname)
      // need to send back the Pid
      remoteSend(reply, newPid)
      run

    case SpawnObject(reply: RemotePid, data: Array[byte]) =>
      //val sr = new StringReader(data)
      //service.serializer.deserialize(sr) match {

      service.serializer.deserialize(data) match {
        case RA(actor) =>
          val newPid = register(actor)
          //Console.println("Spawned a new " + newProc + " (" + newPid + ")")
          actor.start
          // need to send back the Pid
          remoteSend(reply, newPid)
      }
      run

    case NamedSend(sym: Symbol, data) =>
      // look-up name
      nameTable.get(sym) match {
        case None =>
          // message is lost
          Console.println("lost message " + data + " because " + sym + " not registered.")
        case Some(localId) =>
          // deserialize data
          //val sr = new StringReader(data)
          //val msg = service.serializer.deserialize(sr)

          val msg = service.serializer.deserialize(data)
          localSend(localId, msg)
      }
      run

    case Send(pid: RemotePid, data) =>
      // deserialize data
      //val sr = new StringReader(data)
      //val msg = service.serializer.deserialize(sr)
      val msg = service.serializer.deserialize(data)

      Console.println("locally send " + msg + " to " + pid)
      localSend(pid, msg)
      run

    case Link(from:RemotePid, to:RemotePid) =>
      // assume from is local
      linkFromLocal(from, to)
      run

    case UnLink(from:RemotePid, to:RemotePid) =>
      // assume from is local
      unlinkFromLocal(from, to)
      run

    case Exit1(from:RemotePid, to:RemotePid, reason) =>
      // check if "to" traps exit signals
      // if so send a message
      if (trapExits.contains(to.localId))
        // convert signal into message
        localSend(to, Exit1(from, to, reason))
      else
        if (reason.name.equals("normal")) {
          // ignore signal
        }
        else
          exit(from, to, reason)
      run
  }*/

  // TODO
  /*def isReachable(remoteNode: Node): boolean = {
    val pingMsg = new Ping(node)
    val sw = new StringWriter
    service.serializer.serialize(pingMsg, sw)
    service.send(remoteNode, sw.toString())
  }*/

  def processMsg(msg: AnyRef): Unit = synchronized {
    msg match {
      case Spawn(reply: RemotePid, pname) =>
        val newPid = spawn(pname)
        // need to send back the Pid
        remoteSend(reply, newPid)

      case SpawnObject(reply: RemotePid, data) =>
	//val sr = new StringReader(data)
        //service.serializer.deserialize(sr) match {

        service.serializer.deserialize(data) match {
          case RA(actor) =>
            val newPid = register(actor)
            //Console.println("Spawned a new " + newProc + " (" + newPid + ")")
            actor.start
            // need to send back the Pid
            remoteSend(reply, newPid)
        }

      case NamedSend(sym: Symbol, data) =>
        // look-up name
        nameTable.get(sym) match {
          case None =>
            // message is lost
            //Console.println("lost message " + msg + " because " + sym + " not registered.")
          case Some(localId) =>
            // deserialize data
            //val sr = new StringReader(data)
            //val msg = service.serializer.deserialize(sr)
            val msg = service.serializer.deserialize(data)
            localSend(localId, msg)
        }

      case Send(pid: RemotePid, data) =>
        // deserialize data
        //val sr = new StringReader(data)
        //val msg = service.serializer.deserialize(sr)
        val msg = service.serializer.deserialize(data)
        //Console.println("locally send " + msg + " to " + pid)
        localSend(pid, msg)

      case Link(from:RemotePid, to:RemotePid) =>
        // assume from is local
        linkFromLocal(from, to)

      case UnLink(from:RemotePid, to:RemotePid) =>
        // assume from is local
        unlinkFromLocal(from, to)

      case Exit1(from:RemotePid, to:RemotePid, reason) =>
        // check if "to" traps exit signals
        // if so send a message
        if (trapExits.contains(to.localId)) {
          // convert signal into message
          // TODO: simpler message (w/o to) for actor!
          localSend(to, Exit1(from, to, reason))
        }
        else {
          if (reason.name.equals("normal")) {
            // ignore signal
          }
          else
            exit(from, to, reason)
        }
    }
  }

  /* Registers an instance of a remote actor inside this NetKernel.
   */
  def register(newProc: RemoteActor): RemotePid = synchronized {
    newProc.kernel = this
    val newPid = service.createPid(newProc)
    rtable += newPid.localId -> newProc
    pidTable += newProc -> newPid
    newPid
  }

  // local spawn
  def spawn(pname: String): RemotePid = synchronized {
    // get constructor out of table
    (ptable.get(pname)) match {
      case None =>
        //error("No constructor found. Cannot start process.")
        //null

        val newProc = Class.forName(pname).newInstance().asInstanceOf[RemoteActor]
        // create Pid for remote communication and register process
        val newPid = register(newProc)
        //Console.println("Spawned a new " + newProc + " (" + newPid + ")")
        newProc.start
        newPid

      case Some(cons) =>
        val newProc = cons()
        // create Pid for remote communication and register process
        val newPid = register(newProc)
        //Console.println("Spawned a new " + newProc + " (" + newPid + ")")
        newProc.start
        newPid
    }
  }

  // local spawn
  def spawn(name: String, arg: RemotePid): RemotePid = synchronized {
    val newPid = spawn(name)
    localSend(newPid, arg)
    newPid
  }

  // assume this.node != node
  def spawn(replyTo: RemotePid, node: Node, a: RemoteActor): Unit = {
    val ra = RA(a)
    //val rsw = new StringWriter
    //service.serializer.serialize(ra, rsw)
    val bytes = service.serializer.serialize(ra)
    //sendToNode(node, SpawnObject(replyTo, rsw.toString()))
    sendToNode(node, SpawnObject(replyTo, bytes))
  }


  def registerSerializer(index: String, rep: Serializer => AnyRef) = {
    service.serializer.addRep(index, rep)

    // send registering requests to remote nodes
  }

  // remote spawn
  def spawn(replyTo: RemotePid, node: Node, name: String): RemotePid = synchronized {
    // check if actor is to be spawned locally
    if (node == this.node) {
      val newPid = spawn(name)
      newPid
    }
    else {
      sendToNode(node, Spawn(replyTo, name))
      null  // pid needs to be sent back
    }
  }

  /* Spawns a new actor (locally), executing "fun".
   */
  def spawn(fun: RemoteActor => Unit): RemotePid = synchronized {
    val newProc = new RemoteActor {
      override def run: unit =
        fun(this);
    }

    // create Pid for remote communication and register process
    val newPid = register(newProc)
    //Console.println("Spawned a new " + newProc + " (" + newPid + ")")
    newProc.start
    newPid
  }

  /* Spawns a new actor (locally), executing "fun".
   */
  def spawnLink(pid: RemotePid, fun: RemoteActor => unit): RemotePid = synchronized {
    val newProc = new RemoteActor {
      override def run: unit =
        fun(this);
    }

    // create Pid for remote communication and register process
    val newPid = register(newProc)
    //Console.println("Spawned a new " + newProc + " (" + newPid + ")")

    // link new process to pid (assume pid is local)
    link(pid, newPid)

    newProc.start
    newPid
  }

  // maps local ids to their linked pids
  private val links = new HashMap[int,HashSet[RemotePid]];

  // which of the local processes traps exit signals?
  private val trapExits = new HashSet[int];

  def processFlag(pid: RemotePid, flag: Symbol, set: Boolean) = synchronized {
    if (flag.name.equals("trapExit")) {
      if (trapExits.contains(pid.localId) && !set)
        trapExits -= pid.localId
      else if (!trapExits.contains(pid.localId) && set)
        trapExits += pid.localId
    }
  }

  // assume from.node == this.node
  private def unlinkFromLocal(from: RemotePid, to: RemotePid): Unit =
    links.get(from.localId) match {
      case None =>
        // has no links -> ignore
      case Some(set) =>
        set -= to
        if (set.size == 0) links -= from.localId
    };

  /*
   unlinks bi-directional link
   assume from.node == this.node
   */
  def unlink(from: RemotePid, to: RemotePid): Unit = synchronized {
    unlinkFromLocal(from, to)
    if (to.node == this.node)
      unlinkFromLocal(to, from)
    else
      // (2) send message to NetKernel of "to" to unlink a
      // uni-directional link from "to" to "from"
      sendToNode(to.node, UnLink(to, from))
  }

  // assume from.node == this.node
  private def linkFromLocal(from: RemotePid, to: RemotePid): Unit =
    // TODO: send Exit to from if to is invalid
    links.get(from.localId) match {
      case None =>
        // from has no links, yet
        val linksTo = new HashSet[RemotePid]
        linksTo += to
        links += from.localId -> linksTo
      case Some(set) =>
        set += to
    };

  /*
   creates bi-directional link
   assume from.node == this.node
   */
  def link(from: RemotePid, to: RemotePid): unit = synchronized {
    // (1) create locally a uni-directional link
    linkFromLocal(from, to)
    if (to.node == this.node)
      linkFromLocal(to, from)
    else
      // (2) send message to NetKernel of "to" to create a
      // uni-directional link from "to" to "from"
      sendToNode(to.node, Link(to, from))
  }

  // Assume "to" is local.
  def exit(from: RemotePid, to: RemotePid, reason: Symbol): unit = {
    // remove link
    unlinkFromLocal(to, from)
    exit(to, reason)
  }

  val exitMarks = new HashSet[RemotePid]

  /*
   If reason is unequal to 'normal then
   this will cause all linked processes to
   (transitively) terminate abnormally.

   Assume pid is local.
   */
  def exit(pid: RemotePid, reason: Symbol): Unit = synchronized {
    if (!(exitMarks contains pid)) {
      exitMarks += pid  // mark pid as exiting
      //Console.println("" + pid + " is exiting (" + reason + ").")

      // first look-up remote actor in rtable
      val actor = rtable(pid.localId)
      // remove from table of running processes
      rtable -= pid.localId
      // remove from pid table
      pidTable -= actor

      // send exit signals to linked processes
      links.get(pid.localId) match {
        case None =>
          //Console.println("no linked processes")

        case Some(set) =>  // set of remote pids that we want to terminate
          //Console.println("sending exit signals to linked processes")

          val iter = set.elements
          while (iter.hasNext) {
            val linkedPid = iter.next

            unlinkFromLocal(pid, linkedPid)

            if (linkedPid.node == this.node) {
              unlinkFromLocal(linkedPid, pid)

              if (trapExits.contains(linkedPid.localId))
                localSend(linkedPid, Exit1(pid, linkedPid, reason))
              else if (!reason.name.equals("normal"))
                exit(linkedPid, reason)
            }
            else
              sendToNode(linkedPid.node,
                         Exit1(pid, linkedPid, reason))
          }
          exitMarks -= pid
      }
    }
  }

  private val monNodes =
    new HashMap[Node,HashMap[RemotePid,Int]]

  def monitorNode(client: RemotePid, mnode: Node, cond: Boolean) = synchronized {
    monNodes.get(mnode) match {
      case None =>
        // nobody is monitoring this node
        if (cond) {
          val map = new HashMap[RemotePid,int]
          map += client -> 1
          monNodes += mnode -> map
        }
      case Some(map) =>
        map.update(client, map(client) + (if (cond) 1 else -1))
    }

    // if no connection exists:
    // try connecting, if it fails deliver nodedown msg
    if (cond && !service.isConnected(mnode)) {
      try {
        service.connect(mnode)
      }
      catch {
        case uhe: UnknownHostException =>
          nodeDown(mnode)
        case ioe: IOException =>
          nodeDown(mnode)
        case se: SecurityException =>
          nodeDown(mnode)
      }
    }
  }

  def nodeDown(mnode: Node) =
    // send NodeDown msg to registered RemotePids
    monNodes.get(mnode) match {
      case None =>
        // nobody is monitoring this node
      case Some(map) =>
        // iterate over keys (RemotePids of interested clients)
        val iter = map.keys
        while (iter.hasNext) {
          val client = iter.next
          for (val i <- List.range(0, map(client))) {
            // send nodedown msg
            client ! Pair(NodeDown(), mnode)
          }
        }
    }

}
