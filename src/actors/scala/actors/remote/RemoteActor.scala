/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.actors.remote


/** <p>
 *    This object provides methods for creating, registering, and
 *    selecting remotely accessible actors.
 *  </p>
 *  <p>
 *    A remote actor is typically created like this:
 *  </p><pre>
 *  actor {
 *    alive(9010)
 *    register('myName, self)
 *
 *    // behavior
 *  }
 *  </pre>
 *  <p>
 *    It can be accessed by an actor running on a (possibly)
 *    different node by selecting it in the following way:
 *  </p><pre>
 *  actor {
 *    // ...
 *    <b>val</b> c = select(TcpNode("127.0.0.1", 9010), 'myName)
 *    c ! msg
 *    // ...
 *  }
 *  </pre>
 *
 *  @author Philipp Haller
 */
object RemoteActor {

  private val kernels = new scala.collection.mutable.HashMap[Actor, NetKernel]

  /**
   * Makes <code>self</code> remotely accessible on TCP port
   * <code>port</code>.
   */
  def alive(port: int): unit = synchronized {
    val serv = new TcpService(port)
    serv.start()
    kernels += Actor.self -> serv.kernel
  }

  /**
   * Registers <code>a</code> under <code>name</code> on this
   * node.
   */
  def register(name: Symbol, a: Actor): unit = synchronized {
    val kernel = kernels.get(Actor.self) match {
      case None =>
        val serv = new TcpService(TcpService.generatePort)
        serv.start()
        kernels += Actor.self -> serv.kernel
        serv.kernel
      case Some(k) =>
        k
    }
    kernel.register(name, a)
  }

  private def selfKernel = kernels.get(Actor.self) match {
    case None =>
      // establish remotely accessible
      // return path (sender)
      val serv = new TcpService(TcpService.generatePort)
      serv.start()
      kernels += Actor.self -> serv.kernel
      serv.kernel
    case Some(k) =>
      k
  }

  /**
   * Returns (a proxy for) the actor registered under
   * <code>name</code> on <code>node</code>.
   */
  def select(node: Node, sym: Symbol): Actor =
    new Actor {
      def act() {}
      override def !(msg: Any): Unit = msg match {
        case a: AnyRef =>
          selfKernel.send(node, sym, a)
        case other =>
          error("Cannot send non-AnyRef value remotely.")
      }
      override def !?(msg: Any): Any = msg match {
        case a: AnyRef =>
          val replyCh = Actor.self.freshReplyChannel
          selfKernel.syncSend(node, sym, a)
          replyCh.receive {
            case x => x
          }
        case other =>
          error("Cannot send non-AnyRef value remotely.")
      }
    }
}


/**
 * This class represents a machine node on a TCP network.
 *
 * @author Philipp Haller
 */
case class Node(address: String, port: Int)
