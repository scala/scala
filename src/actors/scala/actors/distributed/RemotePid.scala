/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors.distributed

import scala.actors.multi.ExcHandlerDesc

import java.io._

/**
 * @author Philipp Haller
 */
[serializable]
abstract class RemotePid(locId: int, kern: NetKernel, actor: RemoteActor) extends scala.actors.multi.Process {
  def this() = this(0, null, null) // for serialization

  private var _locId = locId

  override def equals(that: Any) = that match {
    case rpid: RemotePid =>
      (this.node == rpid.node && this.localId == rpid.localId)
    case _ => false
  }

  //[throws(classOf[IOException])]
  private def writeObject(out: ObjectOutputStream): Unit = {
    //Console.println("writing locID"+locId)
    out.writeInt(locId)
  }

  //[throws(classOf[IOException]), throws(classOf[ClassNotFoundException])]
  private def readObject(in: ObjectInputStream): Unit = {
    _locId = in.readInt()
    //Console.println("read _locID"+_locId)
  }

  //[throws(classOf[ObjectStreamException])]
  private def readResolve(): AnyRef = {
    Console.println("readResolve")
    null
    //build nothing. Subclasses will do...
  }

  def node: Node;

  def localId: int = locId;

  def kernel = kern;

  override def !(msg: Any): unit = {
    //Console.println("! " + msg)
    if (actor != null)
      actor send msg
    else
      kernel.remoteSend(this, msg)
  }

  override def link(other: Process): Unit =
    other match {
      case rpid: RemotePid =>
        kernel.link(this, rpid)
    };

  //TODO (dont know if this is local to kernel.node)
  override def linkTo(other: Process): Unit =
    other match {
      case rpid: RemotePid =>
        // do nothing
    };

  override def unlink(other: Process): Unit =
    other match {
      case rpid: RemotePid =>
        kernel.unlink(this, rpid)
    };

  //TODO (dont know if this is local to kernel.node)
  override def unlinkFrom(other: Process): Unit =
    other match {
      case rpid: RemotePid =>
        // do nothing
    };

  override def exit(reason: Symbol): Unit = kernel.exit(this, reason);
  override def exit(from: Process, reason: Symbol): unit = {
    from match {
      case rpid: RemotePid =>
        kernel.exit(rpid, reason);
    }
  }

  override def handleExc(destDesc: ExcHandlerDesc, e: Throwable): unit = {}
}

[serializable] case class TcpPid(n: TcpNode, locId: int, kern: NetKernel, actor: RemoteActor) extends RemotePid(locId, kern, actor) {
  def node: TcpNode = n;

  private var _locId = locId
  private var _node = n

  override def equals(that: Any) =
    super.equals(that)

  //[throws(classOf[IOException])]
  private def writeObject(out: ObjectOutputStream): Unit = {
    out.writeInt(locId)
    out.writeObject(n)
  }

  //[throws(classOf[IOException]), throws(classOf[ClassNotFoundException])]
  private def readObject(in: ObjectInputStream): Unit = {
    _locId = in.readInt()
    _node = in.readObject().asInstanceOf[TcpNode]
  }

  //[throws(classOf[ObjectStreamException])]
  private def readResolve(): AnyRef = {
    val kernel = NetKernel.kernel;
    //TODO val actor = kernel.getLocalRef(_locId)
    TcpPid(_node, _locId, kernel, actor)
  }
}

[serializable] case class JXTAPid(n: JXTANode, locId: int, kern: NetKernel, actor: RemoteActor) extends RemotePid(locId, kern, actor) {
  def node: JXTANode = n;

  private  var _locId = locId
  private  var _node = n

  override def equals(that: Any) =
    super.equals(that)

  //[throws(classOf[IOException])]
  private def writeObject(out: ObjectOutputStream): Unit = {
    out.writeInt(locId)
    out.writeObject(n)
  }

  //[throws(classOf[IOException]), throws(classOf[ClassNotFoundException])]
  private def readObject(in: ObjectInputStream): Unit = {
    _locId = in.readInt()
    _node = in.readObject().asInstanceOf[JXTANode]
  }

  //[throws(classOf[ObjectStreamException])]
  private def readResolve(): AnyRef = {
    val kernel = NetKernel.kernel;
    //TODO val actor = kernel.getLocalRef(_locId)
    JXTAPid(_node, _locId, kernel, actor)
  }
}

//================================================================================

object CaseTest {

  def getBytes(obj: AnyRef): Array[byte] = {
    val bos = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bos)
    out.writeObject(obj)
    out.flush()
    bos.toByteArray()
  }

  def getObject(a: Array[byte]): AnyRef = {
    val bis = new ByteArrayInputStream(a)
    val in = new ObjectInputStream(bis)
    val obj = in.readObject()
    obj
  }

  def main(args: Array[String]): Unit = {
    val node = JXTANode ("test node");
    val pid1 = JXTAPid (node, 4, null, null);
    val pid2 = JXTAPid (node, 4, new NetKernel(null), null);

    Console.println("node Before: " + node)
    Console.println("node After : " + getObject(getBytes(node)))

    Console.println("pid1 Before: " + pid1)
    Console.println("pid1 After : " + getObject(getBytes(pid1)))

    Console.println("pid2 Before: " + pid2)
    Console.println("pid2 After : " + getObject(getBytes(pid2)))

    Console.println("pid2 After : " + getObject((new String (getBytes(pid2))).getBytes))
  }
}
