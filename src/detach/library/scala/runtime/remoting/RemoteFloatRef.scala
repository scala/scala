/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: RemoteFloatRef.scala 18398 2009-07-28 14:26:36Z michelou $

package scala.runtime.remoting

import java.rmi.server.{UnicastRemoteObject, Unreferenced}
import scala.runtime.{FloatRef, RemoteRef}

/**
 * The trait Remote<code>RemoteFloatRef</code> provides a remote interface
 * for manipulating float references.
 *
 * @author Stephane Micheloud
 * @version 1.0
 */
@remote
trait RemoteFloatRef {
  def elem_=(value: Float)
  def elem: Float
}

/**
 * The class <code>RemoteFloatRefImpl</code> implements a remote (global)
 * float reference by inheriting from the class
 * <code>UnicastRemoteObject</code>.
 *
 * In particular, it forwards method invocations to the <code>elem</code>
 * accessors of class <code>runtime.FloatRef</code> and implements the
 * <code>java.rmi.server.Unreferenced</code> interface.
 *
 * @author Stephane Micheloud
 * @version 1.0
 */
class RemoteFloatRefImpl(name: String, x: FloatRef)
extends UnicastRemoteObject with RemoteFloatRef with Unreferenced {
  def elem_=(value: Float) { x.elem = value }
  def elem: Float = x.elem
  override def toString() = x.elem.toString
  def unreferenced() {
    Debug.info("[RemoteIntFloatImpl] unreferenced: "+this)
    RemoteRef.unbind(name)
  }
}
