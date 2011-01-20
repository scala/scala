/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: RemoteIntRef.scala 18398 2009-07-28 14:26:36Z michelou $

package scala.runtime.remoting

import java.rmi.server.{UnicastRemoteObject, Unreferenced}
import scala.runtime.{IntRef, RemoteRef}

/**
 * The trait Remote<code>RemoteIntRef</code> provides a remote interface
 * for manipulating integer references.
 *
 * @author Stephane Micheloud
 * @version 1.0
 */
@remote
trait RemoteIntRef {
  def elem_=(value: Int)
  def elem: Int
}

/**
 * The class <code>RemoteIntRefImpl</code> implements a remote (global)
 * integer reference by inheriting from the class
 * <code>UnicastRemoteObject</code>.
 *
 * In particular, it forwards method invocations to the <code>elem</code>
 * accessors of class <code>runtime.IntRef</code> and implements the
 * <code>java.rmi.server.Unreferenced</code> interface to automatically
 * remove the no more referenced binding from the registry.
 *
 * @author Stephane Micheloud
 * @version 1.0
 */
class RemoteIntRefImpl(name: String, x: IntRef)
extends UnicastRemoteObject with RemoteIntRef with Unreferenced {
  def elem_=(value: Int) { x.elem = value }
  def elem: Int = x.elem
  override def toString() = x.elem.toString
  def unreferenced() {
    Debug.info("[RemoteIntRefImpl] unreferenced: "+this)
    RemoteRef.unbind(name)
  }
}
