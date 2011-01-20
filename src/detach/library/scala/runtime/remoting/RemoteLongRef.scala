/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: RemoteLongRef.scala 18398 2009-07-28 14:26:36Z michelou $

package scala.runtime.remoting

import java.rmi.server.{UnicastRemoteObject, Unreferenced}
import scala.runtime.{LongRef, RemoteRef}

/**
 * The trait Remote<code>RemoteLongRef</code> provides a remote interface
 * for manipulating long integer references.
 *
 * @author Stephane Micheloud
 * @version 1.0
 */
@remote
trait RemoteLongRef {
  def elem_=(value: Long)
  def elem: Long
}

/**
 * The class <code>RemoteLongRefImpl</code> implements a remote (global)
 * long integer reference by inheriting from the class
 * <code>UnicastRemoteObject</code>.
 *
 * In particular, it forwards method invocations to the <code>elem</code>
 * accessors of class <code>runtime.LongRef</code> and implements the
 * <code>java.rmi.server.Unreferenced</code> interface to automatically
 * remove the no more referenced binding from the registry.
 *
 * @author Stephane Micheloud
 * @version 1.0
 */
class RemoteLongRefImpl(name: String, x: LongRef)
extends UnicastRemoteObject with RemoteLongRef with Unreferenced {
  def elem_=(value: Long) { x.elem = value }
  def elem: Long = x.elem
  override def toString() = x.elem.toString
  def unreferenced() {
    Debug.info("[RemoteLongRefImpl] unreferenced: "+this)
    RemoteRef.unbind(name)
  }
}
