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
 *    <b>val</b> c = select(Node("127.0.0.1", 9010), 'myName)
 *    c ! msg
 *    // ...
 *  }
 *  </pre>
 *
 * @version 0.9.9
 * @author Philipp Haller
 */
object RemoteActor {

  private val kernels = new scala.collection.mutable.HashMap[Actor, NetKernel]

  /**
   * Makes <code>self</code> remotely accessible on TCP port
   * <code>port</code>.
   */
  def alive(port: Int): Unit = synchronized {
    val serv = TcpService(port)
    val kern = serv.kernel
    val s = Actor.self
    kernels += s -> kern
    Debug.info("registering kill handler")
    s.kill = () => {
      Debug.info("alive actor "+s+" terminated")
      kernels -= s
      if (kernels.isEmpty) {
        Debug.info("interrupting "+serv)
        // terminate TcpService
        serv.interrupt()
      }
    }
  }

  /**
   * Registers <code>a</code> under <code>name</code> on this
   * node.
   */
  def register(name: Symbol, a: Actor): Unit = synchronized {
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
    selfKernel.getOrCreateProxy(node, sym)
}


/**
 * This class represents a machine node on a TCP network.
 *
 * @param address the host name, or <code>null</code> for the loopback address.
 * @param port    the port number.
 *
 * @author Philipp Haller
 */
case class Node(address: String, port: Int)
