/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: RegistryDelegate.scala 18234 2009-07-07 13:21:57Z michelou $

package scala.runtime.remoting

import java.rmi.{RMISecurityManager, Remote, RemoteException}
import java.rmi.registry.{LocateRegistry, Registry}
import java.rmi.server.UnicastRemoteObject

/**
 * <p>
 *   This class implements the registry delegate concept
 *   (see http://www.genady.net/rmi/v20/docs/delegate/RegistryDelegate.html)
 * </p>
 * <p>
 *   In order to enforce some level of security, the standard RMI registry
 *   implementation (e.g. <code>rmiregistry.exe</code>) only allows processes
 *   on the same host to register objects in the registry (think of a bank
 *   running a registry on one of its servers, and doesn't want anybody
 *   modifying it). So, by design, if a process tries to
 *   <code>bind(String, Remote)</code> an object to a remote registry,
 *   an exception will be thrown.
 * </p>
 * <p>
 *   However, the design of a distributed system may require remote clients to
 *   register themselves in a central registry. If such system is deployed in a
 *   controlled and trusted environment (e.g., a firewalled intranet with tight
 *   access control), the security risk may be acceptable.
 * </p>
 * <p>
 *   The simplest technical solution to the remote registration problem is to
 *   have a registry delegate. A registry delegate is an object that serves as
 *   a proxy for the real registry. The delegate itself usually appears in the
 *   registry under a well known name. It implements the Registry interface and
 *   simply delegates all method calls to the appropriate methods of the real
 *   registry. The delegate is allowed to perform bind and unbind operations
 *   because it is running on the same host as the registry.
 * </p>
 * <p>
 *   The common scenario for starting a registry and creating the delegate is
 *   starting a class with the following <code>main(Array[String])</code> method:
 * </p>
 * <pre>
 *   @throws(classOf[AccessException], classOf[RemoteException], classOf[AlreadyBoundException])
 *   <b>object</b> namingService {
 *     <b>def</b> main(args: Array[String]) {
 *       <b>if</b> (System.getSecurityManager() == <b>null</b>)
 *         System.setSecurityManager(<b>new</b> RMISecurityManager())
 *
 *       <b>val</b> registry = LocateRegistry.createRegistry(REGISTRY_PORT)
 *       registry.bind(DELEGATE_NAME, <b>new</b> RegistryDelegate());
 *
 *       do {
 *         <b>try</b> {
 *           Thread.sleep(Long.MAX_VALUE)
 *         } <b>catch</b> {
 *           <b>case</b> e: InterruptedException => // do nothing
 *           <b>case</b> e: Throwable => e.printStackTrace(); System.exit(1)
 *         }
 *       } while (<b>true</b>)
 *     }
 *  }</pre>
 * <p>
 *   The common usage scenario looks something like:
 * </p><pre>
 *   Registry remoteRegistry = LocateRegistry.getRegistry("remotehost.mycompany.com");
 *   Registry delegate = (Registry) remoteRegistry.lookup(DELEGATE_NAME);
 *   delegate.bind("someName", <b>new</b> SomeRemoteObject());</pre>
 * <p>
 *   The <code>getRegistryDelegate(String)</code> method is a helper method
 *   that fetches the registry delegate for you.
 * </p>
 * <p>
 *   The <code>main(Array[String])</code> method of this class will create a
 *   local registry on the default port, create a registry delegate and bind
 *   it under the well known name that you chose in the wizard
 *   (<code>DELEGATE_NAME</code>).
 * </p>
 *
 * @author Genady Beryozkin, rmi-info@genady.net
 */

object RMIDelegate {
  /** The name under which the delegate appears in the registry. */
  val DELEGATE_NAME = "foo"

  /** This method retrieves the registry delegate from a registry that is
   *  running on a remote host.
   */
  @throws(classOf[RemoteException])
  def getRegistryDelegate(remoteHost: String): Registry =
    getRegistryDelegate(remoteHost, Registry.REGISTRY_PORT)

  /** This method retrieves the registry delegate from a registry that is
   * running on a remote host.
   */
  @throws(classOf[RemoteException])
  def getRegistryDelegate(remoteHost: String, remotePort: Int): Registry = {
    val registry = LocateRegistry.getRegistry(remoteHost, remotePort)
    (registry lookup DELEGATE_NAME).asInstanceOf[Registry]
  }

  /** A simple way to run a registry and bind a registry delegate. */
  @throws(classOf[RemoteException])
  def main(args: Array[String]) {
    var port = Registry.REGISTRY_PORT

    if (args.length > 0) {
      if (args(0) equals "-help") {
        println("Usage: rmidelegate <options> <port>")
        exit(0)
      }
      try {
        port = args(0).toInt
      } catch {
        case e: NumberFormatException =>
          println("Usage: rmidelegate <options> <port>")
         exit(1)
      }
      val opts = args filter (_ startsWith "-J-D")
      for (opt <- opts) {
        val x = opt.substring(4) split "="
        if (x.length == 2) System.setProperty(x(0), x(1))
        else System.setProperty(x(0), "")
      }
    }

    if (System.getSecurityManager() == null)
      System.setSecurityManager(new RMISecurityManager() {
        override def checkPermission(p: java.security.Permission) {}
      })


    val registry = LocateRegistry.createRegistry(port)
    registry.bind(DELEGATE_NAME, new RegistryDelegate())

    do {
      try {
        Thread.sleep(Long.MaxValue)
      } catch {
       case e: InterruptedException =>
         // do nothing
       case e: Throwable =>
         e.printStackTrace()
         exit(1)
      }
    } while (true)
  }

}

/** Create a delegate for a user provided registry instance. The registry is
 *  assumed to be a local registry, as there is no point in creating a delegate
 *  for a remote registry.
 */
class RegistryDelegate(reg: Registry) extends UnicastRemoteObject with Registry {
  /** The local registry */
  private val localRegistry: Registry = reg

  /** Create a delegate for a local registry that is bound to the default
   *  local port (1099).
   */
  def this() = this(LocateRegistry.getRegistry())

  /** Create a delegate for a local registry that is bound to a user
   *  specified port.
   */
  def this(port: Int) = this(LocateRegistry.getRegistry(port))

  @throws(classOf[RemoteException])
  def bind(name: String, obj: Remote) { localRegistry.bind(name, obj) }

  @throws(classOf[RemoteException])
  def list(): Array[String] = localRegistry.list()

  @throws(classOf[RemoteException])
  def lookup(name: String): Remote = localRegistry.lookup(name)

  @throws(classOf[RemoteException])
  def rebind(name: String, obj: Remote) { localRegistry.rebind(name, obj) }

  @throws(classOf[RemoteException])
  def unbind(name: String) { localRegistry.unbind(name) }

}
