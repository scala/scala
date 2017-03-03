/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

/**
 * An annotation that designates the class to which it is applied as remotable.
 *
 * For instance, the Scala code
 * {{{
 * @remote trait Hello {
 *   def sayHello(): String
 * }
 * }}}
 * is equivalent to the following Java code:
 * {{{
 * public interface Hello extends java.rmi.Remote {
 *     String sayHello() throws java.rmi.RemoteException;
 * }
 * }}}
 */
@deprecated("extend java.rmi.Remote instead and add @throws[java.rmi.RemoteException] to public methods", "2.12.0")
class remote extends scala.annotation.StaticAnnotation {}
