/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package mutable

import scala.annotation.nowarn
import scala.collection.generic.DefaultSerializable

/** This class implements mutable sets using a hashtable.
 *  The iterator and all traversal methods of this class visit elements in the order they were inserted.
 *
 *  @tparam A     the type of the elements contained in this set.
 *
 *  @define Coll `LinkedHashSet`
 *  @define coll linked hash set
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 *  @define orderDependent
 *  @define orderDependentFold
 */
class LinkedHashSet[A]
  extends AbstractSet[A]
    with SetOps[A, LinkedHashSet, LinkedHashSet[A]]
    with StrictOptimizedIterableOps[A, LinkedHashSet, LinkedHashSet[A]]
    with IterableFactoryDefaults[A, LinkedHashSet]
    with DefaultSerializable  {

  override def iterableFactory: IterableFactory[LinkedHashSet] = LinkedHashSet

  // stepper is not overridden to use XTableStepper because that stepper would not return the
  // elements in insertion order

  type Entry = LinkedHashSet.Entry[A]

  @transient protected var firstEntry: Entry = null
  @transient protected var lastEntry: Entry = null
  @transient private[this] var table: HashTable[A, AnyRef, Entry] = newHashTable

  // Used by scala-java8-compat (private[mutable] erases to public, so Java code can access it)
  private[mutable] def getTable: HashTable[A, AnyRef, Entry] = table

  private def newHashTable =
    new HashTable[A, AnyRef, Entry] {
      def createNewEntry(key: A, value: AnyRef) = {
        val e = new Entry(key)
        if (firstEntry eq null) firstEntry = e
        else { lastEntry.later = e; e.earlier = lastEntry }
        lastEntry = e
        e
      }
      override def foreachEntry[U](f: Entry => U): Unit = {
        var cur = firstEntry
        while (cur ne null) {
          f(cur): Unit
          cur = cur.later
        }
      }
    }

  override def last: A =
    if (size > 0) lastEntry.key
    else throw new NoSuchElementException("Cannot call .last on empty LinkedHashSet")
      
  override def lastOption: Option[A] =
    if (size > 0) Some(lastEntry.key)
    else None

  override def head: A =
    if (size > 0) firstEntry.key
    else throw new NoSuchElementException("Cannot call .head on empty LinkedHashSet")
      
  override def headOption: Option[A] =
    if (size > 0) Some(firstEntry.key)
    else None

  override def size: Int = table.tableSize
  override def knownSize: Int = size
  override def isEmpty: Boolean = size == 0
  def contains(elem: A): Boolean = table.findEntry(elem) ne null

  def addOne(elem: A): this.type = {
    table.findOrAddEntry(elem, null): Unit
    this
  }

  def subtractOne(elem: A): this.type = {
    remove(elem): Unit
    this
  }

  override def remove(elem: A): Boolean = {
    val e = table.removeEntry(elem)
    if (e eq null) false
    else {
      if (e.earlier eq null) firstEntry = e.later
      else e.earlier.later = e.later
      if (e.later eq null) lastEntry = e.earlier
      else e.later.earlier = e.earlier
      e.earlier = null // Null references to prevent nepotism
      e.later = null
      true
    }
  }

  def iterator: Iterator[A] = new AbstractIterator[A] {
    private[this] var cur = firstEntry
    def hasNext = cur ne null
    def next() =
      if (hasNext) { val res = cur.key; cur = cur.later; res }
      else Iterator.empty.next()
  }

  override def foreach[U](f: A => U): Unit = {
    var cur = firstEntry
    while (cur ne null) {
      f(cur.key): Unit
      cur = cur.later
    }
  }

  override def clear(): Unit = {
    table.clearTable()
    firstEntry = null
    lastEntry = null
  }

  private def writeObject(out: java.io.ObjectOutputStream): Unit = {
    out.defaultWriteObject()
    table.serializeTo(out, { e => out.writeObject(e.key) })
  }

  private def readObject(in: java.io.ObjectInputStream): Unit = {
    in.defaultReadObject()
    table = newHashTable
    table.init(in, table.createNewEntry(in.readObject().asInstanceOf[A], null))
  }

  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
  override protected[this] def stringPrefix = "LinkedHashSet"
}

/** $factoryInfo
 *  @define Coll `LinkedHashSet`
 *  @define coll linked hash set
 */
@SerialVersionUID(3L)
object LinkedHashSet extends IterableFactory[LinkedHashSet] {

  override def empty[A]: LinkedHashSet[A] = new LinkedHashSet[A]

  def from[E](it: collection.IterableOnce[E]) =
    it match {
      case lhs: LinkedHashSet[E] => lhs
      case _ => Growable.from(empty[E], it)
    }

  def newBuilder[A] = new GrowableBuilder(empty[A])

  /** Class for the linked hash set entry, used internally.
   */
  private[mutable] final class Entry[A](val key: A) extends HashEntry[A, Entry[A]] {
    var earlier: Entry[A] = null
    var later: Entry[A] = null
  }
}

