/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: RemoteBooleanRef.scala 18398 2009-07-28 14:26:36Z michelou $

package scala.runtime.remoting

import java.rmi.server.{UnicastRemoteObject, Unreferenced}
import scala.runtime.{BooleanRef, RemoteRef}

/**
 * The trait Remote<code>RemoteBooleanRef</code> provides a remote interface
 * for manipulating boolean references.
 *
 * @author Stephane Micheloud
 * @version 1.0
 */
@remote
trait RemoteBooleanRef {
  def elem_=(value: Boolean)
  def elem: Boolean
}

/**
 * The class <code>RemoteBooleanRefImpl</code> implements a remote (global)
 * boolean reference by inheriting from the class
 * <code>UnicastRemoteObject</code>.
 *
 * In particular, it forwards method invocations to the <code>elem</code>
 * accessors of class <code>runtime.BooleanRef</code> and implements the
 * <code>java.rmi.server.Unreferenced</code> interface to automatically
 * remove the no more referenced binding from the registry.
 *
 * @author Stephane Micheloud
 * @version 1.0
 */
class RemoteBooleanRefImpl(name: String, x: BooleanRef)
extends UnicastRemoteObject with RemoteBooleanRef with Unreferenced {
  def elem_=(value: Boolean) { x.elem = value }
  def elem: Boolean = x.elem
  override def toString() = x.elem.toString
  def unreferenced() {
    Debug.info("[RemoteBooleanRefImpl] unreferenced: "+this)
    RemoteRef.unbind(name)
  }
}
