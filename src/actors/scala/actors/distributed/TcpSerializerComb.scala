/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors.distributed

import java.io.Reader
import scala.io.BytePickle._

import scala.actors.distributed.MessagesComb._
import scala.actors.distributed.NodeComb._
import scala.collection.mutable.HashMap

/**
 * @author Philipp Haller
 */
//TODO: change Service to NetKernel in Serializer interface
class TcpSerializerComb(serv: Service) extends Serializer(serv) {

  private def lookup(typename: String): PU[AnyRef] = {
    val op = table.get(typename)
    op match {
      case None =>
        error("No type representation found.")
        null
      case Some(rep) =>
        val repr = rep.asInstanceOf[PU[AnyRef]]
        repr
    }
  }

  private def lookup(r: Reader): PU[AnyRef] = {
    // read length of type name
    val carr = new Array[char](8)
    r.read(carr)
    val len = Util.decode(new String(carr))
    val content = new Array[char](len)
    r.read(content)
    lookup(new String(content))
  }

  def pid: SPU[RemotePid] = {
    val nodeIntPU = wrap((p: Pair[TcpNode,int]) => TcpPid(p._1, p._2, serv.kernel,
                                                      if (p._1 == serv.node) serv.kernel.getLocalRef(p._2)
                                                      else null),
                         (t: TcpPid) => Pair(t.node, t.localId),
                         pair(tcpNodePU, nat));

    wrap((p:RemotePid) => p, (pid:RemotePid) => pid match {
      case tpid: TcpPid =>
        tpid
      case other =>
        error("no instance of TcpPid!!")
    }, nodeIntPU)
  }

  def anyRef: SPU[AnyRef] =
    wrap((typename: String) => Class.forName(typename).newInstance().asInstanceOf[AnyRef],
         (obj: AnyRef) => Util.baseName(obj),
         string);

  def actorPU: SPU[RA] =
    wrap((typename: String) => RA(Class.forName(typename).newInstance().asInstanceOf[RemoteActor]),
         (obj: RA) => Util.baseName(obj.a),
         string);

  val log = new Debug("TcpSerializerComb")
  log.level = 3

  val table = new HashMap[String, AnyRef]

  initialize
  def initialize = {
    table += "int" -> nat
    table += "Send" -> sendPU(this)
    table += "Spawn" -> spawnPU(this)
    table += "TcpNode" -> tcpNodePU
    table += "TcpPid" -> pid
    table += "Exit" -> exitPU(this)
    table += "AnyRef" -> anyRef

    table += "RA" -> actorPU
    table += "SpawnObject" -> spawnObjectPU(this)

    //table += "Incr" -> incrPU(this)
    //table += "Value" -> valuePU(this)
    //table += "Result" -> resultPU(this)
  }

  def addRep(name: String, repCons: Serializer => AnyRef) =
    table.update(name, repCons(this))

  def +=(name: String) =
    new InternalMapTo(name)

  class InternalMapTo(name: String) {
    def ->(repCons: Serializer => AnyRef): unit =
      table.update(name, repCons(TcpSerializerComb.this))
  }

  def serialize(o: AnyRef): Array[byte] = {
    log.info("pickling value of type " + Util.baseName(o))
    val op = table.get(Util.baseName(o))
    op match {
      case None => error("No type representation for " + Util.baseName(o) + " found. Cannot serialize.");
      case Some(rep) =>
        // first write type name
        val bytes = pickle(string, Util.baseName(o))
        val repr = rep.asInstanceOf[SPU[AnyRef]]
        log.info("using type representation " + repr)
        val res = repr.appP(o, new PicklerState(bytes, new PicklerEnv)).stream
        res
    }

  }

  def deserialize(bytes: Array[byte]): AnyRef = {
    val ups = string.appU(new UnPicklerState(bytes, new UnPicklerEnv))
    val typename = ups._1
    table.get(typename) match {
      case None => error("No type representation for " + typename + " found. Cannot deserialize.")
      case Some(rep) =>
        val repr = rep.asInstanceOf[SPU[AnyRef]];
        val obj = repr.appU(ups._2)._1
        log.info("unpickling successful")
        obj
    }
  }
}
