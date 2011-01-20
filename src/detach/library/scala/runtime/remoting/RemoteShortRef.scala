/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: RemoteShortRef.scala 18398 2009-07-28 14:26:36Z michelou $

package scala.runtime.remoting

import java.rmi.server.{UnicastRemoteObject, Unreferenced}
import scala.runtime.{ShortRef, RemoteRef}

/**
 * The trait Remote<code>RemoteShortRef</code> provides a remote interface
 * for manipulating short integer references.
 *
 * @author Stephane Micheloud
 * @version 1.0
 */
@remote
trait RemoteShortRef {
  def elem_=(value: Short)
  def elem: Short
}

/**
 * The class <code>RemoteShortRefImpl</code> implements a remote (global)
 * short integer reference by inheriting from the class
 * <code>UnicastRemoteObject</code>.
 *
 * In particular, it forwards method invocations to the <code>elem</code>
 * accessors of class <code>runtime.ShortRef</code> and implements the
 * <code>java.rmi.server.Unreferenced</code> interface.
 *
 * @author Stephane Micheloud
 * @version 1.0
 */
class RemoteShortRefImpl(name: String, x: ShortRef)
extends UnicastRemoteObject with RemoteShortRef with Unreferenced {
  def elem_=(value: Short) { x.elem = value }
  def elem: Short = x.elem
  override def toString() = x.elem.toString
  def unreferenced() {
    Debug.info("[RemoteShortRefImpl] unreferenced: "+this)
    RemoteRef.unbind(name)
  }
}
