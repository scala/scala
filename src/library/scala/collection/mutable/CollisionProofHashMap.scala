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

import scala.{unchecked => uc}
import scala.annotation.{implicitNotFound, tailrec, unused}
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.generic.DefaultSerializationProxy
import scala.runtime.Statics

/** This class implements mutable maps using a hashtable with red-black trees in the buckets for good
  * worst-case performance on hash collisions. An `Ordering` is required for the element type. Equality
  * as determined by the `Ordering` has to be consistent with `equals` and `hashCode`. Universal equality
  * of numeric types is not supported (similar to `AnyRefMap`).
  *
  * @see [[https://docs.scala-lang.org/overviews/collections-2.13/concrete-mutable-collection-classes.html#hash-tables "Scala's Collection Library overview"]]
  * section on `Hash Tables` for more information.
  *
  * @define Coll `mutable.CollisionProofHashMap`
  * @define coll mutable collision-proof hash map
  * @define mayNotTerminateInf
  * @define willNotTerminateInf
  */
final class CollisionProofHashMap[K, V](initialCapacity: Int, loadFactor: Double)(implicit ordering: Ordering[K])
  extends AbstractMap[K, V]
    with MapOps[K, V, Map, CollisionProofHashMap[K, V]] //--
    with StrictOptimizedIterableOps[(K, V), Iterable, CollisionProofHashMap[K, V]]
    with StrictOptimizedMapOps[K, V, Map, CollisionProofHashMap[K, V]] { //--

  private[this] final def sortedMapFactory: SortedMapFactory[CollisionProofHashMap] = CollisionProofHashMap

  def this()(implicit ordering: Ordering[K]) = this(CollisionProofHashMap.defaultInitialCapacity, CollisionProofHashMap.defaultLoadFactor)(ordering)

  import CollisionProofHashMap.Node
  private[this] type RBNode = CollisionProofHashMap.RBNode[K, V]
  private[this] type LLNode = CollisionProofHashMap.LLNode[K, V]

  /** The actual hash table. */
  private[this] var table: Array[Node] = new Array[Node](tableSizeFor(initialCapacity))

  /** The next size value at which to resize (capacity * load factor). */
  private[this] var threshold: Int = newThreshold(table.length)

  private[this] var contentSize = 0

  override def size: Int = contentSize

  @`inline` private[this] final def computeHash(o: K): Int = {
    val h = if(o.asInstanceOf[AnyRef] eq null) 0 else o.hashCode
    h ^ (h >>> 16)
  }

  @`inline` private[this] final def index(hash: Int) = hash & (table.length - 1)

  override protected def fromSpecific(coll: IterableOnce[(K, V)] @uncheckedVariance): CollisionProofHashMap[K, V] @uncheckedVariance = CollisionProofHashMap.from(coll)
  override protected def newSpecificBuilder: Builder[(K, V), CollisionProofHashMap[K, V]] @uncheckedVariance = CollisionProofHashMap.newBuilder[K, V]

  override def empty: CollisionProofHashMap[K, V] = new CollisionProofHashMap[K, V]

  override def contains(key: K): Boolean = findNode(key) ne null

  def get(key: K): Option[V] = findNode(key) match {
    case null => None
    case nd => Some(nd match {
      case nd: LLNode @uc => nd.value
      case nd: RBNode @uc => nd.value
    })
  }

  @throws[NoSuchElementException]
  override def apply(key: K): V = findNode(key) match {
    case null => default(key)
    case nd => nd match {
      case nd: LLNode @uc => nd.value
      case nd: RBNode @uc => nd.value
    }
  }

  override def getOrElse[V1 >: V](key: K, default: => V1): V1 = {
    val nd = findNode(key)
    if (nd eq null) default else nd match {
      case nd: LLNode @uc => nd.value
      case n => n.asInstanceOf[RBNode].value
    }
  }

  @`inline` private[this] def findNode(elem: K): Node = {
    val hash = computeHash(elem)
    table(index(hash)) match {
      case null => null
      case n: LLNode @uc => n.getNode(elem, hash)
      case n => n.asInstanceOf[RBNode].getNode(elem, hash)
    }
  }

  override def sizeHint(size: Int): Unit = {
    val target = tableSizeFor(((size + 1).toDouble / loadFactor).toInt)
    if(target > table.length) {
      if(size == 0) reallocTable(target)
      else growTable(target)
    }
  }

  override def update(key: K, value: V): Unit = put0(key, value, getOld = false)

  override def put(key: K, value: V): Option[V] = put0(key, value, getOld = true) match {
    case null => None
    case sm => sm
  }

  def addOne(elem: (K, V)): this.type = { put0(elem._1, elem._2, getOld = false); this }

  @`inline` private[this] def put0(key: K, value: V, getOld: Boolean): Some[V] = {
    if(contentSize + 1 >= threshold) growTable(table.length * 2)
    val hash = computeHash(key)
    val idx = index(hash)
    put0(key, value, getOld, hash, idx)
  }

  private[this] def put0(key: K, value: V, getOld: Boolean, hash: Int, idx: Int): Some[V] = {
    val res = table(idx) match {
      case n: RBNode @uc =>
        insert(n, idx, key, hash, value)
      case _old =>
        val old: LLNode = _old.asInstanceOf[LLNode]
        if(old eq null) {
          table(idx) = new LLNode(key, hash, value, null)
        } else {
          var remaining = CollisionProofHashMap.treeifyThreshold
          var prev: LLNode = null
          var n = old
          while((n ne null) && n.hash <= hash && remaining > 0) {
            if(n.hash == hash && key == n.key) {
              val old = n.value
              n.value = value
              return (if(getOld) Some(old) else null)
            }
            prev = n
            n = n.next
            remaining -= 1
          }
          if(remaining == 0) {
            treeify(old, idx)
            return put0(key, value, getOld, hash, idx)
          }
          if(prev eq null) table(idx) = new LLNode(key, hash, value, old)
          else prev.next = new LLNode(key, hash, value, prev.next)
        }
        true
    }
    if(res) contentSize += 1
    if(res) Some(null.asInstanceOf[V]) else null //TODO
  }

  private[this] def treeify(old: LLNode, idx: Int): Unit = {
    table(idx) = CollisionProofHashMap.leaf(old.key, old.hash, old.value, red = false, null)
    var n: LLNode = old.next
    while(n ne null) {
      val root = table(idx).asInstanceOf[RBNode]
      insertIntoExisting(root, idx, n.key, n.hash, n.value, root)
      n = n.next
    }
  }

  override def addAll(xs: IterableOnce[(K, V)]): this.type = {
    sizeHint(xs, delta = contentSize)
    super.addAll(xs)
  }

  // returns the old value or Statics.pfMarker if not found
  private[this] def remove0(elem: K) : Any = {
    val hash = computeHash(elem)
    val idx = index(hash)
    table(idx) match {
      case null => Statics.pfMarker
      case t: RBNode @uc =>
        val v = delete(t, idx, elem, hash)
        if(v.asInstanceOf[AnyRef] ne Statics.pfMarker) contentSize -= 1
        v
      case nd: LLNode @uc if nd.hash == hash && nd.key == elem =>
        // first element matches
        table(idx) = nd.next
        contentSize -= 1
        nd.value
      case nd: LLNode @uc =>
        // find an element that matches
        var prev = nd
        var next = nd.next
        while((next ne null) && next.hash <= hash) {
          if(next.hash == hash && next.key == elem) {
            prev.next = next.next
            contentSize -= 1
            return next.value
          }
          prev = next
          next = next.next
        }
        Statics.pfMarker
    }
  }

  private[this] abstract class MapIterator[R] extends AbstractIterator[R] {
    protected[this] def extract(node: LLNode): R
    protected[this] def extract(node: RBNode): R

    private[this] var i = 0
    private[this] var node: Node = null
    private[this] val len = table.length

    def hasNext: Boolean = {
      if(node ne null) true
      else {
        while(i < len) {
          val n = table(i)
          i += 1
          n match {
            case null =>
            case n: RBNode @uc =>
              node = CollisionProofHashMap.minNodeNonNull(n)
              return true
            case n: LLNode @uc =>
              node = n
              return true
          }
        }
        false
      }
    }

    def next(): R =
      if(!hasNext) Iterator.empty.next()
      else node match {
        case n: RBNode @uc =>
          val r = extract(n)
          node = CollisionProofHashMap.successor(n )
          r
        case n: LLNode @uc =>
          val r = extract(n)
          node = n.next
          r
      }
  }

  override def keysIterator: Iterator[K] = {
    if (isEmpty) Iterator.empty
    else new MapIterator[K] {
      protected[this] def extract(node: LLNode) = node.key
      protected[this] def extract(node: RBNode) = node.key
    }
  }

  override def iterator: Iterator[(K, V)] = {
    if (isEmpty) Iterator.empty
    else new MapIterator[(K, V)] {
      protected[this] def extract(node: LLNode) = (node.key, node.value)
      protected[this] def extract(node: RBNode) = (node.key, node.value)
    }
  }

  private[this] def growTable(newlen: Int) = {
    var oldlen = table.length
    table = java.util.Arrays.copyOf(table, newlen)
    threshold = newThreshold(table.length)
    while(oldlen < newlen) {
      var i = 0
      while (i < oldlen) {
        val old = table(i)
        if(old ne null) splitBucket(old, i, i + oldlen, oldlen)
        i += 1
      }
      oldlen *= 2
    }
  }

  @`inline` private[this] def reallocTable(newlen: Int) = {
    table = new Array(newlen)
    threshold = newThreshold(table.length)
  }

  @`inline` private[this] def splitBucket(tree: Node, lowBucket: Int, highBucket: Int, mask: Int): Unit = tree match {
    case t: LLNode @uc => splitBucket(t, lowBucket, highBucket, mask)
    case t: RBNode @uc => splitBucket(t, lowBucket, highBucket, mask)
  }

  private[this] def splitBucket(list: LLNode, lowBucket: Int, highBucket: Int, mask: Int): Unit = {
    val preLow: LLNode = new LLNode(null.asInstanceOf[K], 0, null.asInstanceOf[V], null)
    val preHigh: LLNode = new LLNode(null.asInstanceOf[K], 0, null.asInstanceOf[V], null)
    //preLow.next = null
    //preHigh.next = null
    var lastLow: LLNode = preLow
    var lastHigh: LLNode = preHigh
    var n = list
    while(n ne null) {
      val next = n.next
      if((n.hash & mask) == 0) { // keep low
        lastLow.next = n
        lastLow = n
      } else { // move to high
        lastHigh.next = n
        lastHigh = n
      }
      n = next
    }
    lastLow.next = null
    if(list ne preLow.next) table(lowBucket) = preLow.next
    if(preHigh.next ne null) {
      table(highBucket) = preHigh.next
      lastHigh.next = null
    }
  }

  private[this] def splitBucket(tree: RBNode, lowBucket: Int, highBucket: Int, mask: Int): Unit = {
    var lowCount, highCount = 0
    tree.foreachNode((n: RBNode) => if((n.hash & mask) != 0) highCount += 1 else lowCount += 1)
    if(highCount != 0) {
      if(lowCount == 0) {
        table(lowBucket) = null
        table(highBucket) = tree
      } else {
        table(lowBucket) = fromNodes(new CollisionProofHashMap.RBNodesIterator(tree).filter(n => (n.hash & mask) == 0), lowCount)
        table(highBucket) = fromNodes(new CollisionProofHashMap.RBNodesIterator(tree).filter(n => (n.hash & mask) != 0), highCount)
      }
    }
  }

  private[this] def tableSizeFor(capacity: Int) =
    (Integer.highestOneBit((capacity-1).max(4))*2).min(1 << 30)

  private[this] def newThreshold(size: Int) = (size.toDouble * loadFactor).toInt

  override def clear(): Unit = {
    java.util.Arrays.fill(table.asInstanceOf[Array[AnyRef]], null)
    contentSize = 0
  }

  override def remove(key: K): Option[V] = {
    val v = remove0(key)
    if(v.asInstanceOf[AnyRef] eq Statics.pfMarker) None else Some(v.asInstanceOf[V])
  }

  def subtractOne(elem: K): this.type = { remove0(elem); this }

  override def knownSize: Int = size

  override def isEmpty: Boolean = size == 0

  override def foreach[U](f: ((K, V)) => U): Unit = {
    val len = table.length
    var i = 0
    while(i < len) {
      val n = table(i)
      if(n ne null) n match {
        case n: LLNode @uc => n.foreach(f)
        case n: RBNode @uc => n.foreach(f)
      }
      i += 1
    }
  }

  override def foreachEntry[U](f: (K, V) => U): Unit = {
    val len = table.length
    var i = 0
    while(i < len) {
      val n = table(i)
      if(n ne null) n match {
        case n: LLNode @uc => n.foreachEntry(f)
        case n: RBNode @uc => n.foreachEntry(f)
      }
      i += 1
    }
  }

  protected[this] def writeReplace(): AnyRef = new DefaultSerializationProxy(new CollisionProofHashMap.DeserializationFactory[K, V](table.length, loadFactor, ordering), this)

  override protected[this] def className = "CollisionProofHashMap"

  override def getOrElseUpdate(key: K, defaultValue: => V): V = {
    val hash = computeHash(key)
    val idx = index(hash)
    table(idx) match {
      case null => ()
      case n: LLNode @uc =>
        val nd = n.getNode(key, hash)
        if(nd != null) return nd.value
      case n =>
        val nd = n.asInstanceOf[RBNode].getNode(key, hash)
        if(nd != null) return nd.value
    }
    val table0 = table
    val default = defaultValue
    if(contentSize + 1 >= threshold) growTable(table.length * 2)
    // Avoid recomputing index if the `defaultValue()` or new element hasn't triggered a table resize.
    val newIdx = if (table0 eq table) idx else index(hash)
    put0(key, default, getOld = false, hash, newIdx)
    default
  }

  ///////////////////// Overrides code from SortedMapOps

  /** Builds a new `CollisionProofHashMap` by applying a function to all elements of this $coll.
    *
    *  @param f      the function to apply to each element.
    *  @return       a new $coll resulting from applying the given function
    *                `f` to each element of this $coll and collecting the results.
    */
  def map[K2, V2](f: ((K, V)) => (K2, V2))
      (implicit @implicitNotFound(CollisionProofHashMap.ordMsg) ordering: Ordering[K2]): CollisionProofHashMap[K2, V2] =
    sortedMapFactory.from(new View.Map[(K, V), (K2, V2)](this, f))

  /** Builds a new `CollisionProofHashMap` by applying a function to all elements of this $coll
    *  and using the elements of the resulting collections.
    *
    *  @param f      the function to apply to each element.
    *  @return       a new $coll resulting from applying the given collection-valued function
    *                `f` to each element of this $coll and concatenating the results.
    */
  def flatMap[K2, V2](f: ((K, V)) => IterableOnce[(K2, V2)])
      (implicit @implicitNotFound(CollisionProofHashMap.ordMsg) ordering: Ordering[K2]): CollisionProofHashMap[K2, V2] =
    sortedMapFactory.from(new View.FlatMap(this, f))

  /** Builds a new sorted map by applying a partial function to all elements of this $coll
    *  on which the function is defined.
    *
    *  @param pf     the partial function which filters and maps the $coll.
    *  @return       a new $coll resulting from applying the given partial function
    *                `pf` to each element on which it is defined and collecting the results.
    *                The order of the elements is preserved.
    */
  def collect[K2, V2](pf: PartialFunction[(K, V), (K2, V2)])
      (implicit @implicitNotFound(CollisionProofHashMap.ordMsg) ordering: Ordering[K2]): CollisionProofHashMap[K2, V2] =
    sortedMapFactory.from(new View.Collect(this, pf))

  override def concat[V2 >: V](suffix: IterableOnce[(K, V2)]): CollisionProofHashMap[K, V2] = sortedMapFactory.from(suffix match {
    case it: Iterable[(K, V2)] => new View.Concat(this, it)
    case _ => iterator.concat(suffix.iterator)
  })

  /** Alias for `concat` */
  @`inline` override final def ++ [V2 >: V](xs: IterableOnce[(K, V2)]): CollisionProofHashMap[K, V2] = concat(xs)

  @deprecated("Consider requiring an immutable Map or fall back to Map.concat", "2.13.0")
  override def + [V1 >: V](kv: (K, V1)): CollisionProofHashMap[K, V1] =
     sortedMapFactory.from(new View.Appended(this, kv))

  @deprecated("Use ++ with an explicit collection argument instead of + with varargs", "2.13.0")
  override def + [V1 >: V](elem1: (K, V1), elem2: (K, V1), elems: (K, V1)*): CollisionProofHashMap[K, V1] =
     sortedMapFactory.from(new View.Concat(new View.Appended(new View.Appended(this, elem1), elem2), elems))

  ///////////////////// RedBlackTree code derived from mutable.RedBlackTree:

  @`inline` private[this] def isRed(node: RBNode) = (node ne null) && node.red
  @`inline` private[this] def isBlack(node: RBNode) = (node eq null) || !node.red

  @unused @`inline` private[this] def compare(key: K, hash: Int, node: LLNode): Int = {
    val i = hash - node.hash
    if(i != 0) i else ordering.compare(key, node.key)
  }

  @`inline` private[this] def compare(key: K, hash: Int, node: RBNode): Int = {
    /*val i = hash - node.hash
    if(i != 0) i else*/ ordering.compare(key, node.key)
  }

  // ---- insertion ----

  @tailrec private[this] final def insertIntoExisting(_root: RBNode, bucket: Int, key: K, hash: Int, value: V, x: RBNode): Boolean = {
    val cmp = compare(key, hash, x)
    if(cmp == 0) {
      x.value = value
      false
    } else {
      val next = if(cmp < 0) x.left else x.right
      if(next eq null) {
        val z = CollisionProofHashMap.leaf(key, hash, value, red = true, x)
        if (cmp < 0) x.left = z else x.right = z
        table(bucket) = fixAfterInsert(_root, z)
        return true
      }
      else insertIntoExisting(_root, bucket, key, hash, value, next)
    }
  }

  private[this] final def insert(tree: RBNode, bucket: Int, key: K, hash: Int, value: V): Boolean = {
    if(tree eq null) {
      table(bucket) = CollisionProofHashMap.leaf(key, hash, value, red = false, null)
      true
    } else insertIntoExisting(tree, bucket, key, hash, value, tree)
  }

  private[this] def fixAfterInsert(_root: RBNode, node: RBNode): RBNode = {
    var root = _root
    var z = node
    while (isRed(z.parent)) {
      if (z.parent eq z.parent.parent.left) {
        val y = z.parent.parent.right
        if (isRed(y)) {
          z.parent.red = false
          y.red = false
          z.parent.parent.red = true
          z = z.parent.parent
        } else {
          if (z eq z.parent.right) {
            z = z.parent
            root = rotateLeft(root, z)
          }
          z.parent.red = false
          z.parent.parent.red = true
          root = rotateRight(root, z.parent.parent)
        }
      } else { // symmetric cases
        val y = z.parent.parent.left
        if (isRed(y)) {
          z.parent.red = false
          y.red = false
          z.parent.parent.red = true
          z = z.parent.parent
        } else {
          if (z eq z.parent.left) {
            z = z.parent
            root = rotateRight(root, z)
          }
          z.parent.red = false
          z.parent.parent.red = true
          root = rotateLeft(root, z.parent.parent)
        }
      }
    }
    root.red = false
    root
  }

  // ---- deletion ----

  // returns the old value or Statics.pfMarker if not found
  private[this] def delete(_root: RBNode, bucket: Int, key: K, hash: Int): Any = {
    var root = _root
    val z = root.getNode(key, hash: Int)
    if (z ne null) {
      val oldValue = z.value
      var y = z
      var yIsRed = y.red
      var x: RBNode = null
      var xParent: RBNode = null

      if (z.left eq null) {
        x = z.right
        root = transplant(root, z, z.right)
        xParent = z.parent
      }
      else if (z.right eq null) {
        x = z.left
        root = transplant(root, z, z.left)
        xParent = z.parent
      }
      else {
        y = CollisionProofHashMap.minNodeNonNull(z.right)
        yIsRed = y.red
        x = y.right

        if (y.parent eq z) xParent = y
        else {
          xParent = y.parent
          root = transplant(root, y, y.right)
          y.right = z.right
          y.right.parent = y
        }
        root = transplant(root, z, y)
        y.left = z.left
        y.left.parent = y
        y.red = z.red
      }

      if (!yIsRed) root = fixAfterDelete(root, x, xParent)
      if(root ne _root) table(bucket) = root
      oldValue
    } else Statics.pfMarker
  }

  private[this] def fixAfterDelete(_root: RBNode, node: RBNode, parent: RBNode): RBNode = {
    var root = _root
    var x = node
    var xParent = parent
    while ((x ne root) && isBlack(x)) {
      if (x eq xParent.left) {
        var w = xParent.right
        // assert(w ne null)

        if (w.red) {
          w.red = false
          xParent.red = true
          root = rotateLeft(root, xParent)
          w = xParent.right
        }
        if (isBlack(w.left) && isBlack(w.right)) {
          w.red = true
          x = xParent
        } else {
          if (isBlack(w.right)) {
            w.left.red = false
            w.red = true
            root = rotateRight(root, w)
            w = xParent.right
          }
          w.red = xParent.red
          xParent.red = false
          w.right.red = false
          root = rotateLeft(root, xParent)
          x = root
        }
      } else { // symmetric cases
        var w = xParent.left
        // assert(w ne null)

        if (w.red) {
          w.red = false
          xParent.red = true
          root = rotateRight(root, xParent)
          w = xParent.left
        }
        if (isBlack(w.right) && isBlack(w.left)) {
          w.red = true
          x = xParent
        } else {
          if (isBlack(w.left)) {
            w.right.red = false
            w.red = true
            root = rotateLeft(root, w)
            w = xParent.left
          }
          w.red = xParent.red
          xParent.red = false
          w.left.red = false
          root = rotateRight(root, xParent)
          x = root
        }
      }
      xParent = x.parent
    }
    if (x ne null) x.red = false
    root
  }

  // ---- helpers ----

  @`inline` private[this] def rotateLeft(_root: RBNode, x: RBNode): RBNode = {
    var root = _root
    val y = x.right
    x.right = y.left

    val xp = x.parent
    if (y.left ne null) y.left.parent = x
    y.parent = xp

    if (xp eq null) root = y
    else if (x eq xp.left) xp.left = y
    else xp.right = y

    y.left = x
    x.parent = y
    root
  }

  @`inline` private[this] def rotateRight(_root: RBNode, x: RBNode): RBNode = {
    var root = _root
    val y = x.left
    x.left = y.right

    val xp = x.parent
    if (y.right ne null) y.right.parent = x
    y.parent = xp

    if (xp eq null) root = y
    else if (x eq xp.right) xp.right = y
    else xp.left = y

    y.right = x
    x.parent = y
    root
  }

  /**
    * Transplant the node `from` to the place of node `to`. This is done by setting `from` as a child of `to`'s previous
    * parent and setting `from`'s parent to the `to`'s previous parent. The children of `from` are left unchanged.
    */
  private[this] def transplant(_root: RBNode, to: RBNode, from: RBNode): RBNode = {
    var root = _root
    if (to.parent eq null) root = from
    else if (to eq to.parent.left) to.parent.left = from
    else to.parent.right = from
    if (from ne null) from.parent = to.parent
    root
  }

  // building

  def fromNodes(xs: Iterator[Node], size: Int): RBNode = {
    val maxUsedDepth = 32 - Integer.numberOfLeadingZeros(size) // maximum depth of non-leaf nodes
    def f(level: Int, size: Int): RBNode = size match {
      case 0 => null
      case 1 =>
        val nn = xs.next()
        val (key, hash, value) = nn match {
          case nn: LLNode @uc => (nn.key, nn.hash, nn.value)
          case nn: RBNode @uc => (nn.key, nn.hash, nn.value)
        }
        new RBNode(key, hash, value, level == maxUsedDepth && level != 1, null, null, null)
      case n =>
        val leftSize = (size-1)/2
        val left = f(level+1, leftSize)
        val nn = xs.next()
        val right = f(level+1, size-1-leftSize)
        val (key, hash, value) = nn match {
          case nn: LLNode @uc => (nn.key, nn.hash, nn.value)
          case nn: RBNode @uc => (nn.key, nn.hash, nn.value)
        }
        val n = new RBNode(key, hash, value, red = false, left, right, null)
        if(left ne null) left.parent = n
        right.parent = n
        n
    }
    f(1, size)
  }
}

/**
  * $factoryInfo
  * @define Coll `mutable.CollisionProofHashMap`
  * @define coll mutable collision-proof hash map
  */
@SerialVersionUID(3L)
object CollisionProofHashMap extends SortedMapFactory[CollisionProofHashMap] {
  private[collection] final val ordMsg = "No implicit Ordering[${K2}] found to build a CollisionProofHashMap[${K2}, ${V2}]. You may want to upcast to a Map[${K}, ${V}] first by calling `unsorted`."

  def from[K : Ordering, V](it: scala.collection.IterableOnce[(K, V)]): CollisionProofHashMap[K, V] = {
    val k = it.knownSize
    val cap = if(k > 0) ((k + 1).toDouble / defaultLoadFactor).toInt else defaultInitialCapacity
    new CollisionProofHashMap[K, V](cap, defaultLoadFactor) ++= it
  }

  def empty[K : Ordering, V]: CollisionProofHashMap[K, V] = new CollisionProofHashMap[K, V]

  def newBuilder[K : Ordering, V]: Builder[(K, V), CollisionProofHashMap[K, V]] = newBuilder(defaultInitialCapacity, defaultLoadFactor)

  def newBuilder[K : Ordering, V](initialCapacity: Int, loadFactor: Double): Builder[(K, V), CollisionProofHashMap[K, V]] =
    new GrowableBuilder[(K, V), CollisionProofHashMap[K, V]](new CollisionProofHashMap[K, V](initialCapacity, loadFactor)) {
      override def sizeHint(size: Int) = elems.sizeHint(size)
    }

  /** The default load factor for the hash table */
  final def defaultLoadFactor: Double = 0.75

  /** The default initial capacity for the hash table */
  final def defaultInitialCapacity: Int = 16

  @SerialVersionUID(3L)
  private final class DeserializationFactory[K, V](val tableLength: Int, val loadFactor: Double, val ordering: Ordering[K]) extends Factory[(K, V), CollisionProofHashMap[K, V]] with Serializable {
    def fromSpecific(it: IterableOnce[(K, V)]): CollisionProofHashMap[K, V] = new CollisionProofHashMap[K, V](tableLength, loadFactor)(ordering) ++= it
    def newBuilder: Builder[(K, V), CollisionProofHashMap[K, V]] = CollisionProofHashMap.newBuilder(tableLength, loadFactor)(using ordering)
  }

  @unused @`inline` private def compare[K, V](key: K, hash: Int, node: LLNode[K, V])(implicit ord: Ordering[K]): Int = {
    val i = hash - node.hash
    if(i != 0) i else ord.compare(key, node.key)
  }

  @`inline` private def compare[K, V](key: K, hash: Int, node: RBNode[K, V])(implicit ord: Ordering[K]): Int = {
    /*val i = hash - node.hash
    if(i != 0) i else*/ ord.compare(key, node.key)
  }

  private final val treeifyThreshold = 8

  // Superclass for RBNode and LLNode to help the JIT with optimizing instance checks, but no shared common fields.
  // Keeping calls monomorphic where possible and dispatching manually where needed is faster.
  sealed abstract class Node

  /////////////////////////// Red-Black Tree Node

  final class RBNode[K, V](var key: K, var hash: Int, var value: V, var red: Boolean, var left: RBNode[K, V], var right: RBNode[K, V], var parent: RBNode[K, V]) extends Node {
    override def toString: String = "RBNode(" + key + ", " + hash + ", " + value + ", " + red + ", " + left + ", " + right + ")"

    @tailrec def getNode(k: K, h: Int)(implicit ord: Ordering[K]): RBNode[K, V] = {
      val cmp = compare(k, h, this)
      if (cmp < 0) {
        if(left ne null) left.getNode(k, h) else null
      } else if (cmp > 0) {
        if(right ne null) right.getNode(k, h) else null
      } else this
    }

    def foreach[U](f: ((K, V)) => U): Unit = {
      if(left ne null) left.foreach(f)
      f((key, value))
      if(right ne null) right.foreach(f)
    }

    def foreachEntry[U](f: (K, V) => U): Unit = {
      if(left ne null) left.foreachEntry(f)
      f(key, value)
      if(right ne null) right.foreachEntry(f)
    }

    def foreachNode[U](f: RBNode[K, V] => U): Unit = {
      if(left ne null) left.foreachNode(f)
      f(this)
      if(right ne null) right.foreachNode(f)
    }
  }

  @`inline` private def leaf[A, B](key: A, hash: Int, value: B, red: Boolean, parent: RBNode[A, B]): RBNode[A, B] =
    new RBNode(key, hash, value, red, null, null, parent)

  @tailrec private def minNodeNonNull[A, B](node: RBNode[A, B]): RBNode[A, B] =
    if (node.left eq null) node else minNodeNonNull(node.left)

  /**
    * Returns the node that follows `node` in an in-order tree traversal. If `node` has the maximum key (and is,
    * therefore, the last node), this method returns `null`.
    */
  private def successor[A, B](node: RBNode[A, B]): RBNode[A, B] = {
    if (node.right ne null) minNodeNonNull(node.right)
    else {
      var x = node
      var y = x.parent
      while ((y ne null) && (x eq y.right)) {
        x = y
        y = y.parent
      }
      y
    }
  }

  private final class RBNodesIterator[A, B](tree: RBNode[A, B])(implicit @unused ord: Ordering[A]) extends AbstractIterator[RBNode[A, B]] {
    private[this] var nextNode: RBNode[A, B] = if(tree eq null) null else minNodeNonNull(tree)

    def hasNext: Boolean = nextNode ne null

    @throws[NoSuchElementException]
    def next(): RBNode[A, B] = nextNode match {
      case null => Iterator.empty.next()
      case node =>
        nextNode = successor(node)
        node
    }
  }

  /////////////////////////// Linked List Node

  private final class LLNode[K, V](var key: K, var hash: Int, var value: V, var next: LLNode[K, V]) extends Node {
    override def toString = s"LLNode($key, $value, $hash) -> $next"

    private[this] def eq(a: Any, b: Any): Boolean =
      if(a.asInstanceOf[AnyRef] eq null) b.asInstanceOf[AnyRef] eq null else a.asInstanceOf[AnyRef].equals(b)

    @tailrec def getNode(k: K, h: Int)(implicit ord: Ordering[K]): LLNode[K, V] = {
      if(h == hash && eq(k, key) /*ord.compare(k, key) == 0*/) this
      else if((next eq null) || (hash > h)) null
      else next.getNode(k, h)
    }

    @tailrec def foreach[U](f: ((K, V)) => U): Unit = {
      f((key, value))
      if(next ne null) next.foreach(f)
    }

    @tailrec def foreachEntry[U](f: (K, V) => U): Unit = {
      f(key, value)
      if(next ne null) next.foreachEntry(f)
    }

    @tailrec def foreachNode[U](f: LLNode[K, V] => U): Unit = {
      f(this)
      if(next ne null) next.foreachNode(f)
    }
  }
}
