/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable


/** This class can be used as an adaptor to create mutable sets from
 *  Java classes that implement interface <code>java.util.Set</code>.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 19/09/2003
 *  @deprecated Use <code>scala.collection.jcl.Set(jmap)</code> instead
 */
@deprecated class JavaSetAdaptor[A](jset: java.util.Set[A]) extends Set[A] {

  def size: Int = jset.size()

  override def isEmpty: Boolean = jset.isEmpty()

  def contains(elem: A): Boolean = jset.contains(elem)

  def elements: Iterator[A] = new Iterator[A] {
    val iter = jset.iterator()
    def hasNext = iter.hasNext()
    def next = iter.next().asInstanceOf[A]
  }

  def +=(elem: A): Unit = { val x = jset.add(elem); }

  def -=(elem: A): Unit = { val x = jset.remove(elem); }

  override def clear(): Unit = jset.clear()

  override def clone(): Set[A] = {
    val res = new HashSet[A]
    res ++= this
    res
  }
}
