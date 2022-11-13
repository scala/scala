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

import scala.annotation.{nowarn, tailrec}
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

  @transient private[this] var table = new Array[Entry](tableSizeFor(LinkedHashSet.defaultinitialSize))

  private[this] var threshold: Int = newThreshold(table.length)

  private[this] def newThreshold(size: Int) = (size.toDouble * LinkedHashSet.defaultLoadFactor).toInt

  private[this] var contentSize = 0

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

  override def size: Int = contentSize

  override def knownSize: Int = size

  override def isEmpty: Boolean = size == 0

  def contains(elem: A): Boolean = findEntry(elem) ne null

  def addOne(elem: A): this.type = {
    if(contentSize + 1 >= threshold) growTable(table.length * 2)
    addElem(elem, computeHash(elem))
    this
  }

  def subtractOne(elem: A): this.type = {
    remove(elem)
    this
  }

  override def remove(elem: A): Boolean = {
    remove0(elem, computeHash(elem))
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
      f(cur.key)
      cur = cur.later
    }
  }

  override def clear(): Unit = {
    java.util.Arrays.fill(table.asInstanceOf[Array[AnyRef]], null)
    contentSize = 0
    firstEntry = null
    lastEntry = null
  }

  private[this] def tableSizeFor(capacity: Int) =
    (Integer.highestOneBit((capacity-1).max(4))*2).min(1 << 30)

  @`inline` private[this] def improveHash(originalHash: Int): Int = {
    originalHash ^ (originalHash >>> 16)
  }

  /** Computes the improved hash of this key */
  @`inline` private[this] def computeHash(o: A): Int = improveHash(o.##)

  @`inline` private[this] def index(hash: Int) = hash & (table.length - 1)

  @`inline` private[this] def findEntry(key: A): Entry = {
    val hash = computeHash(key)
    table(index(hash)) match {
      case null => null
      case nd => nd.findEntry(key, hash)
    }
  }

  /*create a new entry. If table is empty(firstEntry is null), then the
  * new entry will be the firstEntry. If not, just set the new entry to
  * be the lastEntry.
  * */
  private[this] def createNewEntry(key: A, hash: Int): Entry =
    {
          val e = new Entry(key, hash)
        if (firstEntry eq null) firstEntry = e
        else { lastEntry.later = e; e.earlier = lastEntry }
        lastEntry = e
        e
    }

  /*delete the entry from the linkedhashset. set its earlier entry's later entry
  * and later entry's earlier entry correctly.and then set its earlier and later
  * to be null.*/
  private[this] def deleteEntry(e: Entry): Unit =
    {
      if (e.earlier eq null) firstEntry = e.later
      else e.earlier.later = e.later
      if (e.later eq null) lastEntry = e.earlier
      else e.later.earlier = e.earlier
      e.earlier = null // Null references to prevent nepotism
      e.later = null
    }

  /** Adds an element to this set
    * @param elem element to add
    * @param hash the **improved** hash of `elem` (see computeHash)
    */
  private[this] def addElem(elem: A, hash: Int) : Boolean = {
    val idx = index(hash)
    table(idx) match {
      case null =>
        val nnode = createNewEntry(elem, hash)
         table(idx) = nnode
        nnode.next = null
      case old =>
        var prev = null.asInstanceOf[Entry]
        var n = old
        while((n ne null) && n.hash <= hash) {
          if(n.hash == hash && elem == n.key) return false
          prev = n
          n = n.next
        }
         val nnode = createNewEntry(elem, hash)
        if(prev eq null) {
          nnode.next = old
          table(idx) = nnode

        } else {
          nnode.next = prev.next
          prev.next = nnode
        }
    }
    contentSize += 1
    true
  }

  private[this] def remove0(elem: A, hash: Int): Boolean = {
    val idx = index(hash)
    table(idx) match {
      case null => false
      case nd if nd.hash == hash && nd.key == elem =>
        // first element matches
        table(idx) = nd.next
        deleteEntry(nd)
        nd.next = null
        contentSize -= 1
        true
      case nd =>
        // find an element that matches
        var prev = nd
        var next = nd.next
        while((next ne null) && next.hash <= hash) {
          if(next.hash == hash && next.key == elem) {
            prev.next = next.next
            deleteEntry(next)
            next.next = null
            contentSize -= 1
            return true
          }
          prev = next
          next = next.next
        }
        false
    }
  }

  private[this] def growTable(newlen: Int) = {
    if (newlen < 0)
      throw new RuntimeException(s"new hash table size $newlen exceeds maximum")
    var oldlen = table.length
    threshold = newThreshold(newlen)
    if (size == 0) table = new Array(newlen)
    else {
      table = java.util.Arrays.copyOf(table, newlen)
      val preLow = new Entry(null.asInstanceOf[A], 0)
      val preHigh = new Entry(null.asInstanceOf[A], 0)
      // Split buckets until the new length has been reached. This could be done more
      // efficiently when growing an already filled table to more than double the size.
      while (oldlen < newlen) {
        var i = 0
        while (i < oldlen) {
          val old = table(i)
          if (old ne null) {
            preLow.next = null
            preHigh.next = null
            var lastLow = preLow
            var lastHigh = preHigh
            var n = old
            while (n ne null) {
              val next = n.next
              if ((n.hash & oldlen) == 0) { // keep low
                lastLow.next = n
                lastLow = n
              } else { // move to high
                lastHigh.next = n
                lastHigh = n
              }
              n = next
            }
            lastLow.next = null
            if (old ne preLow.next) table(i) = preLow.next
            if (preHigh.next ne null) {
              table(i + oldlen) = preHigh.next
              lastHigh.next = null
            }
          }
          i += 1
        }
        oldlen *= 2
      }
    }
  }

  private[this] def serializeTo(out: java.io.ObjectOutputStream, writeEntry: Entry => Unit): Unit = {
    out.writeInt(contentSize)
    var cur = firstEntry
    while (cur ne null) {
      writeEntry(cur)
      cur = cur.later
    }
  }

  private def writeObject(out: java.io.ObjectOutputStream): Unit = {
    out.defaultWriteObject()
    serializeTo(out, { e => out.writeObject(e.key) })
  }

  private[this] def serializeFrom(in: java.io.ObjectInputStream, readEntry: => A): Unit = {
    val _contentsize = in.readInt()
    assert(_contentsize > 0)
    clear()
    table = new Array(tableSizeFor(_contentsize))
    threshold = newThreshold(table.length)

    var index = 0
    while (index < size) {
      addOne(readEntry)
      index += 1
    }
  }

  private def readObject(in: java.io.ObjectInputStream): Unit = {
    in.defaultReadObject()
    serializeFrom(in, in.readObject().asInstanceOf[A])
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
  private[mutable] final class Entry[A](val key: A, val hash: Int)  {
    var earlier: Entry[A] = null
    var later: Entry[A] = null
    var next: Entry[A] = null

    @tailrec
    final def findEntry(k: A, h: Int): Entry[A] =
      if (h == hash && k == key) this
      else if ((next eq null) || (hash > h)) null
      else next.findEntry(k, h)

    @tailrec
    final def foreach[U](f: A => U): Unit = {
      f(key)
      if (next ne null) next.foreach(f)
    }

    @tailrec
    final def foreachEntry[U](f: A => U): Unit = {
      f(key)
      if (next ne null) next.foreachEntry(f)
    }
  }

 private[collection] final def defaultLoadFactor: Double = 0.75 // corresponds to 75%

  /** The default initial capacity for the hash table */
  private[collection] final def defaultinitialSize: Int = 16
}

