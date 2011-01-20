/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: RemoteCharRef.scala 18398 2009-07-28 14:26:36Z michelou $

package scala.runtime.remoting

import java.rmi.server.{UnicastRemoteObject, Unreferenced}
import scala.runtime.{CharRef, RemoteRef}

/**
 * The trait Remote<code>RemoteCharRef</code> provides a remote interface
 * for manipulating character references.
 *
 * @author Stephane Micheloud
 * @version 1.0
 */
@remote
trait RemoteCharRef {
  def elem_=(value: Char)
  def elem: Char
}

/**
 * The class <code>RemoteCharRefImpl</code> implements a remote (global)
 * character reference by inheriting from the class
 * <code>UnicastRemoteObject</code>.
 *
 * In particular, it forwards method invocations to the <code>elem</code>
 * accessors of class <code>runtime.CharRef</code> and implements the
 * <code>java.rmi.server.Unreferenced</code> interface to automatically
 * remove the no more referenced binding from the registry.
 *
 * @author Stephane Micheloud
 * @version 1.0
 */
class RemoteCharRefImpl(name: String, x: CharRef)
extends UnicastRemoteObject with RemoteCharRef with Unreferenced {
  def elem_=(value: Char) { x.elem = value }
  def elem: Char = x.elem
  override def toString() = x.elem.toString
  def unreferenced() {
    Debug.info("[RemoteCharRefImpl] unreferenced: "+this)
    RemoteRef.unbind(name)
  }
}
