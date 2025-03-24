/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.collection
package mutable

import scala.annotation.tailrec
import scala.collection.Stepper.EfficientSplit
import scala.collection.generic.DefaultSerializationProxy
import scala.util.hashing.MurmurHash3

/** This class implements mutable sets using a hashtable.
  *
  * @see [[https://docs.scala-lang.org/overviews/collections-2.13/concrete-mutable-collection-classes.html#hash-tables "Scala's Collection Library overview"]]
  * section on `Hash Tables` for more information.
  *
  * @define Coll `mutable.HashSet`
  * @define coll mutable hash set
  * @define mayNotTerminateInf
  * @define willNotTerminateInf
  */
final class HashSet[A](initialCapacity: Int, loadFactor: Double)
  extends AbstractSet[A]
    with SetOps[A, HashSet, HashSet[A]]
    with StrictOptimizedIterableOps[A, HashSet, HashSet[A]]
    with IterableFactoryDefaults[A, HashSet]
    with Serializable {

  def this() = this(HashSet.defaultInitialCapacity, HashSet.defaultLoadFactor)

  import HashSet.Node

  /* The Hashset class holds the following invariant:
   * - For each i between  0 and table.length, the bucket at table(i) only contains elements whose hash-index is i.
   * - Every bucket is sorted in ascendent hash order
   * - The sum of the lengths of all buckets is equal to contentSize.
   */
  /** The actual hash table. */
  private[this] var table = new Array[Node[A]](tableSizeFor(initialCapacity))

  /** The next size value at which to resize (capacity * load factor). */
  private[this] var threshold: Int = newThreshold(table.length)

  private[this] var contentSize = 0

  override def size: Int = contentSize

  /** Performs the inverse operation of improveHash. In this case, it happens to be identical to improveHash*/
  @`inline` private[collection] def unimproveHash(improvedHash: Int): Int = improveHash(improvedHash)

  /** Computes the improved hash of an original (`any.##`) hash. */
  private[this] def improveHash(originalHash: Int): Int = {
    // Improve the hash by xoring the high 16 bits into the low 16 bits just in case entropy is skewed towards the
    // high-value bits. We only use the lowest bits to determine the hash bucket. This is the same improvement
    // algorithm as in java.util.HashMap.
    originalHash ^ (originalHash >>> 16)
  }

  /** Computes the improved hash of this element */
  @`inline` private[this] def computeHash(o: A): Int = improveHash(o.##)

  @`inline` private[this] def index(hash: Int) = hash & (table.length - 1)

  override def contains(elem: A): Boolean = findNode(elem) ne null

  @`inline` private[this] def findNode(elem: A): Node[A] = {
    val hash = computeHash(elem)
    table(index(hash)) match {
      case null => null
      case nd => nd.findNode(elem, hash)
    }
  }

  override def sizeHint(size: Int): Unit = {
    val target = tableSizeFor(((size + 1).toDouble / loadFactor).toInt)
    if(target > table.length) growTable(target)
  }

  override def add(elem: A) : Boolean = {
    if(contentSize + 1 >= threshold) growTable(table.length * 2)
    addElem(elem, computeHash(elem))
  }

  override def addAll(xs: IterableOnce[A]): this.type = {
    sizeHint(xs, delta = 0)
    xs match {
      case hs: immutable.HashSet[A] =>
        hs.foreachWithHash((k, h) => addElem(k, improveHash(h)))
        this
      case hs: mutable.HashSet[A] =>
        val iter = hs.nodeIterator
        while (iter.hasNext) {
          val next = iter.next()
          addElem(next.key, next.hash)
        }
        this
      case lhs: mutable.LinkedHashSet[A] =>
        val iter = lhs.entryIterator
        while (iter.hasNext) {
          val next = iter.next()
          addElem(next.key, next.hash)
        }
        this
      case _ => super.addAll(xs)
    }
  }

  override def subtractAll(xs: IterableOnce[A]): this.type = {
    if (size == 0) {
      return this
    }

    xs match {
      case hs: immutable.HashSet[A] =>
        hs.foreachWithHashWhile { (k, h) =>
          remove(k, improveHash(h))
          size > 0
        }
        this
      case hs: mutable.HashSet[A] =>
        val iter = hs.nodeIterator
        while (iter.hasNext) {
          val next = iter.next()
          remove(next.key, next.hash)
          if (size == 0) return this
        }
        this
      case lhs: mutable.LinkedHashSet[A] =>
        val iter = lhs.entryIterator
        while (iter.hasNext) {
          val next = iter.next()
          remove(next.key, next.hash)
          if (size == 0) return this
        }
        this
      case _ => super.subtractAll(xs)
    }
  }

  /** Adds an element to this set
    * @param elem element to add
    * @param hash the **improved** hash of `elem` (see computeHash)
    */
  private[this] def addElem(elem: A, hash: Int) : Boolean = {
    val idx = index(hash)
    table(idx) match {
      case null =>
        table(idx) = new Node(elem, hash, null)
      case old =>
        var prev: Node[A] = null
        var n = old
        while((n ne null) && n.hash <= hash) {
          if(n.hash == hash && elem == n.key) return false
          prev = n
          n = n.next
        }
        if(prev eq null)
          table(idx) = new Node(elem, hash, old)
        else
          prev.next = new Node(elem, hash, prev.next)
    }
    contentSize += 1
    true
  }

  private[this] def remove(elem: A, hash: Int): Boolean = {
    val idx = index(hash)
    table(idx) match {
      case null => false
      case nd if nd.hash == hash && nd.key == elem =>
        // first element matches
        table(idx) = nd.next
        contentSize -= 1
        true
      case nd =>
        // find an element that matches
        var prev = nd
        var next = nd.next
        while((next ne null) && next.hash <= hash) {
          if(next.hash == hash && next.key == elem) {
            prev.next = next.next
            contentSize -= 1
            return true
          }
          prev = next
          next = next.next
        }
        false
    }
  }

  override def remove(elem: A) : Boolean = remove(elem, computeHash(elem))

  private[this] abstract class HashSetIterator[B] extends AbstractIterator[B] {
    private[this] var i = 0
    private[this] var node: Node[A] = null
    private[this] val len = table.length

    protected[this] def extract(nd: Node[A]): B

    def hasNext: Boolean = {
      if(node ne null) true
      else {
        while(i < len) {
          val n = table(i)
          i += 1
          if(n ne null) { node = n; return true }
        }
        false
      }
    }

    def next(): B =
      if(!hasNext) Iterator.empty.next()
      else {
        val r = extract(node)
        node = node.next
        r
      }
  }

  override def iterator: Iterator[A] = new HashSetIterator[A] {
    override protected[this] def extract(nd: Node[A]): A = nd.key
  }

  /** Returns an iterator over the nodes stored in this HashSet */
  private[collection] def nodeIterator: Iterator[Node[A]] = new HashSetIterator[Node[A]] {
    override protected[this] def extract(nd: Node[A]): Node[A] = nd
  }

  override def stepper[S <: Stepper[_]](implicit shape: StepperShape[A, S]): S with EfficientSplit = {
    import convert.impl._
    val s = shape.shape match {
      case StepperShape.IntShape    => new IntTableStepper[Node[A]]   (size, table, _.next, _.key.asInstanceOf[Int],    0, table.length)
      case StepperShape.LongShape   => new LongTableStepper[Node[A]]  (size, table, _.next, _.key.asInstanceOf[Long],   0, table.length)
      case StepperShape.DoubleShape => new DoubleTableStepper[Node[A]](size, table, _.next, _.key.asInstanceOf[Double], 0, table.length)
      case _         => shape.parUnbox(new AnyTableStepper[A, Node[A]](size, table, _.next, _.key,                      0, table.length))
    }
    s.asInstanceOf[S with EfficientSplit]
  }

  private[this] def growTable(newlen: Int) = {
    var oldlen = table.length
    threshold = newThreshold(newlen)
    if(size == 0) table = new Array(newlen)
    else {
      table = java.util.Arrays.copyOf(table, newlen)
      val preLow: Node[A] = new Node(null.asInstanceOf[A], 0, null)
      val preHigh: Node[A] = new Node(null.asInstanceOf[A], 0, null)
      // Split buckets until the new length has been reached. This could be done more
      // efficiently when growing an already filled table to more than double the size.
      while(oldlen < newlen) {
        var i = 0
        while (i < oldlen) {
          val old = table(i)
          if(old ne null) {
            preLow.next = null
            preHigh.next = null
            var lastLow: Node[A] = preLow
            var lastHigh: Node[A] = preHigh
            var n = old
            while(n ne null) {
              val next = n.next
              if((n.hash & oldlen) == 0) { // keep low
                lastLow.next = n
                lastLow = n
              } else { // move to high
                lastHigh.next = n
                lastHigh = n
              }
              n = next
            }
            lastLow.next = null
            if(old ne preLow.next) table(i) = preLow.next
            if(preHigh.next ne null) {
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

  override def filterInPlace(p: A => Boolean): this.type = {
    if (nonEmpty) {
      var bucket = 0

      while (bucket < table.length) {
        var head = table(bucket)

        while ((head ne null) && !p(head.key)) {
          head = head.next
          contentSize -= 1
        }

        if (head ne null) {
          var prev = head
          var next = head.next

          while (next ne null) {
            if (p(next.key)) {
              prev = next
            } else {
              prev.next = next.next
              contentSize -= 1
            }
            next = next.next
          }
        }

        table(bucket) = head
        bucket += 1
      }
    }
    this
  }

  /*
  private[mutable] def checkTable(): Unit = {
    var i = 0
    var count = 0
    var prev: Node[A] = null
    while(i < table.length) {
      var n = table(i)
      prev = null
      while(n != null) {
        count += 1
        assert(index(n.hash) == i)
        if(prev ne null) assert(prev.hash <= n.hash)
        prev = n
        n = n.next
      }
      i += 1
    }
    assert(contentSize == count)
  }
  */

  private[this] def tableSizeFor(capacity: Int) =
    (Integer.highestOneBit((capacity-1).max(4))*2).min(1 << 30)

  private[this] def newThreshold(size: Int) = (size.toDouble * loadFactor).toInt

  def clear(): Unit = {
    java.util.Arrays.fill(table.asInstanceOf[Array[AnyRef]], null)
    contentSize = 0
  }

  override def iterableFactory: IterableFactory[HashSet] = HashSet

  @`inline` def addOne(elem: A): this.type = { add(elem); this }

  @`inline` def subtractOne(elem: A): this.type = { remove(elem); this }

  override def knownSize: Int = size

  override def isEmpty: Boolean = size == 0

  override def foreach[U](f: A => U): Unit = {
    val len = table.length
    var i = 0
    while(i < len) {
      val n = table(i)
      if(n ne null) n.foreach(f)
      i += 1
    }
  }

  protected[this] def writeReplace(): AnyRef = new DefaultSerializationProxy(new HashSet.DeserializationFactory[A](table.length, loadFactor), this)

  override protected[this] def className = "HashSet"

  override def hashCode: Int = {
    val setIterator = this.iterator
    val hashIterator: Iterator[Any] =
      if (setIterator.isEmpty) setIterator
      else new HashSetIterator[Any] {
        var hash: Int = 0
        override def hashCode: Int = hash
        override protected[this] def extract(nd: Node[A]): Any = {
          hash = unimproveHash(nd.hash)
          this
        }
      }
    MurmurHash3.unorderedHash(hashIterator, MurmurHash3.setSeed)
  }
}

/**
  * $factoryInfo
  * @define Coll `mutable.HashSet`
  * @define coll mutable hash set
  */
@SerialVersionUID(3L)
object HashSet extends IterableFactory[HashSet] {

  def from[B](it: scala.collection.IterableOnce[B]): HashSet[B] = {
    val k = it.knownSize
    val cap = if(k > 0) ((k + 1).toDouble / defaultLoadFactor).toInt else defaultInitialCapacity
    new HashSet[B](cap, defaultLoadFactor) ++= it
  }

  def empty[A]: HashSet[A] = new HashSet[A]

  def newBuilder[A]: Builder[A, HashSet[A]] = newBuilder(defaultInitialCapacity, defaultLoadFactor)

  def newBuilder[A](initialCapacity: Int, loadFactor: Double): Builder[A, HashSet[A]] =
    new GrowableBuilder[A, HashSet[A]](new HashSet[A](initialCapacity, loadFactor)) {
      override def sizeHint(size: Int) = elems.sizeHint(size)
    }

  /** The default load factor for the hash table */
  final def defaultLoadFactor: Double = 0.75

  /** The default initial capacity for the hash table */
  final def defaultInitialCapacity: Int = 16

  @SerialVersionUID(3L)
  private final class DeserializationFactory[A](val tableLength: Int, val loadFactor: Double) extends Factory[A, HashSet[A]] with Serializable {
    def fromSpecific(it: IterableOnce[A]): HashSet[A] = new HashSet[A](tableLength, loadFactor) ++= it
    def newBuilder: Builder[A, HashSet[A]] = HashSet.newBuilder(tableLength, loadFactor)
  }

  private[collection] final class Node[K](_key: K, _hash: Int, private[this] var _next: Node[K]) {
    def key: K = _key
    def hash: Int = _hash
    def next: Node[K] = _next
    def next_= (n: Node[K]): Unit = _next = n

    @tailrec
    def findNode(k: K, h: Int): Node[K] =
      if(h == _hash && k == _key) this
      else if((_next eq null) || (_hash > h)) null
      else _next.findNode(k, h)

    @tailrec
    def foreach[U](f: K => U): Unit = {
      f(_key)
      if(_next ne null) _next.foreach(f)
    }

    override def toString = s"Node($key, $hash) -> $next"
  }
}
