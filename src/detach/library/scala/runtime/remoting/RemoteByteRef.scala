/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: RemoteByteRef.scala 18398 2009-07-28 14:26:36Z michelou $

package scala.runtime.remoting

import java.rmi.server.{UnicastRemoteObject, Unreferenced}
import scala.runtime.{ByteRef, RemoteRef}

/**
 * The trait Remote<code>RemoteByteRef</code> provides a remote interface
 * for manipulating byte references.
 *
 * @author Stephane Micheloud
 * @version 1.0
 */
@remote
trait RemoteByteRef {
  def elem_=(value: Byte)
  def elem: Byte
}

/**
 * The class <code>RemoteByteRefImpl</code> implements a remote (global)
 * byte reference by inheriting from the class
 * <code>UnicastRemoteObject</code>.
 *
 * In particular, it forwards method invocations to the <code>elem</code>
 * accessors of class <code>runtime.ByteRef</code> and implements the
 * <code>java.rmi.server.Unreferenced</code> interface to automatically
 * remove the no more referenced binding from the registry.
 *
 * @author Stephane Micheloud
 * @version 1.0
 */
class RemoteByteRefImpl(name: String, x: ByteRef)
extends UnicastRemoteObject with RemoteByteRef with Unreferenced {
  def elem_=(value: Byte) { x.elem = value }
  def elem: Byte = x.elem
  override def toString() = x.elem.toString
  def unreferenced() {
    Debug.info("[RemoteByteRefImpl] unreferenced: "+this)
    RemoteRef.unbind(name)
  }
}
