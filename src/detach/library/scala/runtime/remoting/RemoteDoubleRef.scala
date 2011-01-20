/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: RemoteDoubleRef.scala 18398 2009-07-28 14:26:36Z michelou $

package scala.runtime.remoting

import java.rmi.server.{UnicastRemoteObject, Unreferenced}
import scala.runtime.{DoubleRef, RemoteRef}

/**
 * The trait Remote<code>RemoteDoubleRef</code> provides..
 *
 * @author Stephane Micheloud
 * @version 1.0
 */
@remote
trait RemoteDoubleRef {
  def elem_=(value: Double)
  def elem: Double
}

/**
 * The class <code>RemoteDoubleRefImpl</code> implements a remote (global)
 * double reference by inheriting from the class
 * <code>UnicastRemoteObject</code>.
 *
 * In particular, it forwards method invocations to the <code>elem</code>
 * accessors of class <code>runtime.DoubleRef</code> and implements the
 * <code>java.rmi.server.Unreferenced</code> interface to automatically
 * remove the no more referenced binding from the registry.
 *
 * @author Stephane Micheloud
 * @version 1.0
 */
class RemoteDoubleRefImpl(name: String, x: DoubleRef)
extends UnicastRemoteObject with RemoteDoubleRef with Unreferenced {
  def elem_=(value: Double) { x.elem = value }
  def elem: Double = x.elem
  override def toString() = x.elem.toString
  def unreferenced() {
    Debug.info("[RemoteDoubleRefImpl] unreferenced: "+this)
    RemoteRef.unbind(name)
  }
}
