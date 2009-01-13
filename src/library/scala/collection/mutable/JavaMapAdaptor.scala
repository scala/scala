/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable


/** This class can be used as an adaptor to create mutable maps from
 *  Java classes that implementat the <code>java.util.Map</code> interface.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 21/07/2003
 *  @deprecated Use <code>scala.collection.jcl.Map(jmap)</code> instead
 */
@deprecated class JavaMapAdaptor[A, B](jmap: java.util.Map[A, B]) extends Map[A, B] {

  def size: Int = jmap.size()

  def get(key: A): Option[B] =
    if (jmap.containsKey(key)) Some(jmap.get(key).asInstanceOf[B]) else None

  override def isEmpty: Boolean = jmap.isEmpty()

  override def apply(key: A): B = jmap.get(key).asInstanceOf[B]

  override def contains(key: A): Boolean = jmap.containsKey(key)

  override def isDefinedAt(key: A) = jmap.containsKey(key)

  override def keys: Iterator[A] = new Iterator[A] {
    val iter = jmap.keySet().iterator()
    def hasNext = iter.hasNext()
    def next = iter.next().asInstanceOf[A]
  }

  override def values: Iterator[B] = new Iterator[B] {
    val iter = jmap.values().iterator()
    def hasNext = iter.hasNext()
    def next = iter.next().asInstanceOf[B]
  }

  def elements: Iterator[(A, B)] = new Iterator[(A, B)] {
    val iter = jmap.keySet().iterator()
    def hasNext = iter.hasNext()
    def next = {
      val key = iter.next().asInstanceOf[A]
      (key, apply(key))
    }
  }

  def update(key: A, value: B): Unit = { val x = jmap.put(key, value); }

  def -= (key: A): Unit = { val x = jmap.remove(key); }

  override def clear(): Unit = jmap.clear()

  override def clone(): Map[A, B] = {
    val res = new HashMap[A, B]
    res ++= this
    res
  }
}
