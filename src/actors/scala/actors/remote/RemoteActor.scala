/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.actors.remote


/**
 This object provides methods for creating, registering, and
 selecting remotely accessible actors.

 A remote actor is typically created like this:
 <pre>
 actor {
   alive(9010)
   register('myName, self)

   // behavior
 }
 </pre>

 It can be accessed by an actor running on a (possibly)
 different node by selecting it in the following way:
 <pre>
 actor {
   // ...
   <b>val</b> c = select(TcpNode("127.0.0.1", 9010), 'myName)
   c ! msg
   // ...
 }
 </pre>

 @author Philipp Haller
 */
object RemoteActor {

  private val kernels = new scala.collection.mutable.HashMap[Actor, NetKernel]

  /**
   * Makes <code>self</code> remotely accessible on TCP port
   * <code>port</code>.
   */
  def alive(port: int): Unit = {
    val serv = new TcpService(port)
    serv.start()
    kernels += Actor.self -> serv.kernel
  }

  /**
   * Registers <code>a</code> under <code>name</code> on this
   * node.
   */
  def register(name: Symbol, a: Actor): Unit = {
    val kernel = kernels.get(Actor.self) match {
      case None => {
        val serv = new TcpService(TcpService.generatePort)
        serv.start()
        kernels += Actor.self -> serv.kernel
        serv.kernel
      }
      case Some(k) => k
    }
    kernel.register(name, a)
  }

  /**
   * Returns (a proxy for) the actor registered under
   * <code>name</code> on <code>node</code>.
   */
  def select(node: Node, sym: Symbol): Actor =
    new Actor {
      def act(): Unit = {}
      override def !(msg: Any): Unit = msg match {
        case a: AnyRef => {
          // establish remotely accessible
          // return path (sender)
          val kernel = kernels.get(Actor.self) match {
            case None => {
              val serv = new TcpService(TcpService.generatePort)
              serv.start()
              kernels += Actor.self -> serv.kernel
              serv.kernel
            }
            case Some(k) => k
          }
          kernel.send(node, sym, a)
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
