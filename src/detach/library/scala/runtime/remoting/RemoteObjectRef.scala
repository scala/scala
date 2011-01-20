/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: RemoteObjectRef.scala 18398 2009-07-28 14:26:36Z michelou $

package scala.runtime.remoting

import java.rmi.server.{UnicastRemoteObject, Unreferenced}
import scala.runtime.{ObjectRef, RemoteRef}

/**
 * The trait Remote<code>RemoteObjectRef</code> provides a remote interface
 * for manipulating object references.
 *
 * @author Stephane Micheloud
 * @version 1.0
 */
@remote
trait RemoteObjectRef {
  def elem_=(value: AnyRef)
  def elem: AnyRef
}

/**
 * The class <code>RemoteObjectRefImpl</code> implements a remote (global)
 * object reference by inheriting from the class
 * <code>UnicastRemoteObject</code>.
 *
 * In particular, it forwards method invocations to the <code>elem</code>
 * accessors of class <code>runtime.ObjectRef</code> and implements the
 * <code>java.rmi.server.Unreferenced</code> interface to automatically
 * remove the no more referenced binding from the registry.
 *
 * @author Stephane Micheloud
 * @version 1.0
 */
class RemoteObjectRefImpl(name: String, x: ObjectRef)
extends UnicastRemoteObject with RemoteObjectRef with Unreferenced {
  def elem_=(value: AnyRef) { x.elem = value }
  def elem: AnyRef = x.elem
  override def toString() = x.elem.toString
  def unreferenced() {
    Debug.info("[RemoteObjectRefImpl] unreferenced: "+this)
    RemoteRef.unbind(name)
  }
}
