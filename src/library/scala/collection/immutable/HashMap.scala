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
package immutable

import java.{util => ju}

import generic._
import scala.annotation.unchecked.{uncheckedVariance => uV}
import parallel.immutable.ParHashMap
import scala.util.hashing.MurmurHash3

/** This class implements immutable maps using a hash trie.
 *
 *  '''Note:''' The builder of this hash map may return specialized representations for small maps.
 *
 *  @tparam A      the type of the keys contained in this hash map.
 *  @tparam B      the type of the values associated with the keys.
 *
 *  @author  Martin Odersky
 *  @author  Tiark Rompf
 *  @since   2.3
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#hash-tries "Scala's Collection Library overview"]]
 *  section on `Hash Tries` for more information.
 *  @define Coll `immutable.HashMap`
 *  @define coll immutable hash map
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
@SerialVersionUID(2L)
sealed class HashMap[A, +B] extends AbstractMap[A, B]
                        with Map[A, B]
                        with MapLike[A, B, HashMap[A, B]]
                        with Serializable
                        with CustomParallelizable[(A, B), ParHashMap[A, B]]
                        with HasForeachEntry[A, B]
{
  import HashMap.{nullToEmpty, bufferSize}

  override def size: Int = 0

  override def empty = HashMap.empty[A, B]

  def iterator: Iterator[(A,B)] = Iterator.empty

  override def foreach[U](f: ((A, B)) => U): Unit = ()
  private[immutable] def foreachEntry[U](f: (A, B) => U): Unit = ()
  override def hashCode(): Int = {
    if (isEmpty) MurmurHash3.emptyMapHash
    else {
      val hasher = new Map.HashCodeAccumulator()
      foreachEntry(hasher)
      hasher.finalizeHash
    }
  }

  def get(key: A): Option[B] =
    get0(key, computeHash(key), 0)

  override final def contains(key: A): Boolean =
    contains0(key, computeHash(key), 0)

  override def updated [B1 >: B] (key: A, value: B1): HashMap[A, B1] =
    updated0(key, computeHash(key), 0, value, null, null)

  override def + [B1 >: B] (kv: (A, B1)): HashMap[A, B1] =
    updated0(kv._1, computeHash(kv._1), 0, kv._2, kv, null)

  override def + [B1 >: B] (elem1: (A, B1), elem2: (A, B1), elems: (A, B1) *): HashMap[A, B1] =
    this + elem1 + elem2 ++ elems

  def - (key: A): HashMap[A, B] =
    removed0(key, computeHash(key), 0)

  override def tail: HashMap[A, B] = this - head._1

  override def filter(p: ((A, B)) => Boolean) = {
    val buffer = new Array[HashMap[A, B]](bufferSize(size))
    nullToEmpty(filter0(p, false, 0, buffer, 0))
  }

  override def filterNot(p: ((A, B)) => Boolean) = {
    val buffer = new Array[HashMap[A, B]](bufferSize(size))
    nullToEmpty(filter0(p, true, 0, buffer, 0))
  }

  protected def filter0(p: ((A, B)) => Boolean, negate: Boolean, level: Int, buffer: Array[HashMap[A, B @uV]], offset0: Int): HashMap[A, B] = null

  protected def elemHashCode(key: A) = key.##

  protected final def improve(hcode: Int) = {
    var h: Int = hcode + ~(hcode << 9)
    h = h ^ (h >>> 14)
    h = h + (h << 4)
    h ^ (h >>> 10)
  }

  private[collection] def computeHash(key: A) = improve(elemHashCode(key))

  import HashMap.{Merger, MergeFunction, liftMerger}

  private[collection] def get0(key: A, hash: Int, level: Int): Option[B] = None
  protected def contains0(key: A, hash: Int, level: Int): Boolean = false
  private[collection] def updated0[B1 >: B](key: A, hash: Int, level: Int, value: B1, kv: (A, B1), merger: Merger[A, B1]): HashMap[A, B1] =
    new HashMap.HashMap1(key, hash, value, kv)

  protected def removed0(key: A, hash: Int, level: Int): HashMap[A, B] = this

  protected def writeReplace(): AnyRef = new HashMap.SerializationProxy(this)

  def split: Seq[HashMap[A, B]] = Seq(this)

  /** Creates a new map which is the merge of this and the argument hash map.
   *
   *  Uses the specified collision resolution function if two keys are the same.
   *  The collision resolution function will always take the first argument from
   *  `this` hash map and the second from `that`.
   *
   *  The `merged` method is on average more performant than doing a traversal and reconstructing a
   *  new immutable hash map from scratch.
   *
   *  @tparam B1      the value type of the other hash map
   *  @param that     the other hash map
   *  @param mergef   the merge function or null if the first key-value pair is to be picked
   */
  def merged[B1 >: B](that: HashMap[A, B1])(mergef: MergeFunction[A, B1]): HashMap[A, B1] = merge0(that, 0, liftMerger(mergef))

  protected def merge0[B1 >: B](that: HashMap[A, B1], level: Int, merger: Merger[A, B1]): HashMap[A, B1] = that

  override def par = ParHashMap.fromTrie(this)

  /* Override to avoid tuple allocation in foreach */
  private[collection] class HashMapKeys extends ImmutableDefaultKeySet {
    override def foreach[U](f: A => U) = foreachEntry((key, _) => f(key))
    override lazy val hashCode = super.hashCode()
  }
  override def keySet: immutable.Set[A] = new HashMapKeys

  /** The implementation class of the iterable returned by `values`.
   */
  private[collection] class HashMapValues extends DefaultValuesIterable {
    override def foreach[U](f: B => U) = foreachEntry((_, value) => f(value))
  }
  override def values: scala.collection.Iterable[B] = new HashMapValues

  override final def transform[W, That](f: (A, B) => W)(implicit bf: CanBuildFrom[HashMap[A, B], (A, W), That]): That =
    if ((bf eq Map.canBuildFrom) || (bf eq HashMap.canBuildFrom)) transformImpl(f).asInstanceOf[That]
    else super.transform(f)(bf)

  /* `transform` specialized to return a HashMap */
  protected def transformImpl[W](f: (A, B) => W): HashMap[A, W] = HashMap.empty

  private def isCompatibleCBF(cbf: CanBuildFrom[_,_,_]): Boolean = {
    cbf match {
      case w: WrappedCanBuildFrom[_,_,_] =>
        isCompatibleCBF(w.wrapped)
      case _ =>
        (cbf eq HashMap.canBuildFrom) || (cbf eq Map.canBuildFrom)
    }
  }

  override def ++[B1 >: B](xs: GenTraversableOnce[(A, B1)]): Map[A, B1] = addImpl(xs, HashMap.canBuildFrom[A, B1])

  override def ++[C >: (A, B), That](that: GenTraversableOnce[C])(implicit bf: CanBuildFrom[HashMap[A, B], C, That]): That =
    addImpl(that, bf)
  private def addImpl[C >: (A, B), That](that: GenTraversableOnce[C], bf: CanBuildFrom[HashMap[A, B], C, That]): That = {
    if (isCompatibleCBF(bf)) {
      //here we know that That =:= HashMap[_, _], or compatible with it
      if (this eq that.asInstanceOf[AnyRef]) that.asInstanceOf[That]
      else if (that.isEmpty) this.asInstanceOf[That]
      else that match {
        case thatHash: HashMap[A, B] =>
          //default Merge prefers to keep than replace
          //so we merge from thatHash
          (thatHash.merged(this) (null) ).asInstanceOf[That]
        case that =>
          var result: HashMap[Any, _] = this.asInstanceOf[HashMap[Any, _]]
          that foreach { case kv: (_, _) => result = result + kv }
          result.asInstanceOf[That]
      }
    } else super.++(that)(bf)
  }

  override def ++:[C >: (A, B), That](that: TraversableOnce[C])(implicit bf: CanBuildFrom[HashMap[A, B], C, That]): That = {
    if (isCompatibleCBF(bf)) addSimple(that)
    else super.++:(that)
  }

  override def ++:[C >: (A, B), That](that: scala.Traversable[C])(implicit bf: CanBuildFrom[HashMap[A, B], C, That]): That = {
    if (isCompatibleCBF(bf)) addSimple(that)
    else super.++:(that)
  }
  private def addSimple[C >: (A, B), That](that: TraversableOnce[C]): That = {
    //here we know that That =:= HashMap[_, _], or compatible with it
    if (this eq that.asInstanceOf[AnyRef]) that.asInstanceOf[That]
    else if (that.isEmpty) this.asInstanceOf[That]
    else that match {
      case thatHash: HashMap[A, B] =>
        val merger: Merger[A, B] = HashMap.liftMerger[A, B](null)
        // merger prefers to keep than replace
        // so we invert
        (this.merge0(thatHash, 0, merger.invert)).asInstanceOf[That]

        // cope with Maps that have a foreachEntry ??
      case that =>
        object adder extends Function1[(A,B), Unit] {
          var result: HashMap[A, B] = this.asInstanceOf[HashMap[A, B]]
          val merger = HashMap.liftMerger[A, B](null)

          override def apply(kv: (A, B)): Unit = {
            val key = kv._1
            result = result.updated0(key, computeHash(key), 0, kv._2, kv, merger)
          }
        }
        that.asInstanceOf[scala.Traversable[(A,B)]] foreach adder
        adder.result.asInstanceOf[That]
    }
  }
}

/** $factoryInfo
 *  @define Coll `immutable.HashMap`
 *  @define coll immutable hash map
 *
 *  @author  Tiark Rompf
 *  @since   2.3
 */
object HashMap extends ImmutableMapFactory[HashMap] with BitOperations.Int {

  private[collection] abstract class Merger[A, B] {
    def apply(kv1: (A, B), kv2: (A, B)): (A, B)
    def invert: Merger[A, B]
    def retainIdentical = false
  }

  private type MergeFunction[A1, B1] = ((A1, B1), (A1, B1)) => (A1, B1)

  private def liftMerger[A1, B1](mergef: MergeFunction[A1, B1]): Merger[A1, B1] =
    if (mergef == null) defaultMerger.asInstanceOf[Merger[A1, B1]] else liftMerger0(mergef)

  private val defaultMerger : Merger[Any, Any] = new Merger[Any, Any] {
    override def apply(a: (Any, Any), b: (Any, Any)): (Any, Any) = a
    override def retainIdentical: Boolean = true
    override val invert: Merger[Any, Any] = new Merger[Any, Any] {
      override def apply(a: (Any, Any), b: (Any, Any)): (Any, Any) = b
      override def retainIdentical: Boolean = true
      override def invert = defaultMerger
    }
  }

  private[this] def liftMerger0[A1, B1](mergef: MergeFunction[A1, B1]): Merger[A1, B1] = new Merger[A1, B1] {
    self =>
    def apply(kv1: (A1, B1), kv2: (A1, B1)): (A1, B1) = mergef(kv1, kv2)
    val invert: Merger[A1, B1] = new Merger[A1, B1] {
      def apply(kv1: (A1, B1), kv2: (A1, B1)): (A1, B1) = mergef(kv2, kv1)
      def invert: Merger[A1, B1] = self
    }
  }

  override def newBuilder[A, B]: mutable.Builder[(A, B), HashMap[A, B]] = new HashMapBuilder[A,B]
  private class HashMapBuilder[A, B] extends mutable.MapBuilder[A, B, HashMap[A, B]](HashMap.empty) {
    //not sure if this should be part of MapBuilder
    override def ++=(xs: TraversableOnce[(A, B)]): HashMapBuilder.this.type = {
      elems ++= xs
      this
    }
  }

  /** $mapCanBuildFromInfo */
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), HashMap[A, B]] =
    ReusableCBF.asInstanceOf[CanBuildFrom[Coll, (A, B), HashMap[A, B]]]
  private val ReusableCBF = new MapCanBuildFrom[Nothing, Nothing]
  def empty[A, B]: HashMap[A, B] = EmptyHashMap.asInstanceOf[HashMap[A, B]]

  private object EmptyHashMap extends HashMap[Any, Nothing] {
    override def head: (Any, Nothing) = throw new NoSuchElementException("Empty Map")
    override def tail: HashMap[Any, Nothing] = throw new NoSuchElementException("Empty Map")
  }

  // utility method to create a HashTrieMap from two leaf HashMaps (HashMap1 or HashMapCollision1) with non-colliding hash code)
  private def makeHashTrieMap[A, B](hash0:Int, elem0:HashMap[A, B], hash1:Int, elem1:HashMap[A, B], level:Int, size:Int) : HashTrieMap[A, B] = {
    val index0 = (hash0 >>> level) & 0x1f
    val index1 = (hash1 >>> level) & 0x1f
    if(index0 != index1) {
      val bitmap = (1 << index0) | (1 << index1)
      val elems = new Array[HashMap[A,B]](2)
      if(index0 < index1) {
        elems(0) = elem0
        elems(1) = elem1
      } else {
        elems(0) = elem1
        elems(1) = elem0
      }
      new HashTrieMap[A, B](bitmap, elems, size)
    } else {
      val elems = new Array[HashMap[A,B]](1)
      val bitmap = (1 << index0)
      elems(0) = makeHashTrieMap(hash0, elem0, hash1, elem1, level + 5, size)
      new HashTrieMap[A, B](bitmap, elems, size)
    }
  }

  @deprecatedInheritance("This class will be made final in a future release.", "2.12.2")
  class HashMap1[A,+B](private[collection] val key: A, private[collection] val hash: Int, private[collection] val value: (B @uV), private[collection] var kv: (A,B @uV)) extends HashMap[A,B] {
    override def size = 1

    private[collection] def getKey = key
    private[collection] def getHash = hash
    private[collection] def computeHashFor(k: A) = computeHash(k)

    override def get0(key: A, hash: Int, level: Int): Option[B] =
      if (hash == this.hash && key == this.key) Some(value) else None

    override protected def contains0(key: A, hash: Int, level: Int): Boolean =
      hash == this.hash && key == this.key
    private[collection] override def updated0[B1 >: B](key: A, hash: Int, level: Int, value: B1, kv: (A, B1), merger: Merger[A, B1]): HashMap[A, B1] =
      if (hash == this.hash && key == this.key) {
        if (merger eq null) {
          if (this.value.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) this
          else new HashMap1(key, hash, value, kv)
        } else {
          val current = this.ensurePair
          val nkv = merger(current, if (kv != null) kv else (key, value))
          if ((current eq nkv) || (
            (current._1.asInstanceOf[AnyRef] eq nkv._1.asInstanceOf[AnyRef]) &&
              (current._2.asInstanceOf[AnyRef] eq nkv._2.asInstanceOf[AnyRef]))) this
          else new HashMap1(nkv._1, hash, nkv._2, nkv)
        }
      } else {
        if (hash != this.hash) {
          // they have different hashes, but may collide at this level - find a level at which they don't
          val that = new HashMap1[A, B1](key, hash, value, kv)
          makeHashTrieMap[A, B1](this.hash, this, hash, that, level, 2)
        } else {
          // 32-bit hash collision (rare, but not impossible)
          new HashMapCollision1(hash, ListMap.empty.updated(this.key, this.value).updated(key, value))
        }
      }

    override def removed0(key: A, hash: Int, level: Int): HashMap[A, B] =
      if (hash == this.hash && key == this.key) HashMap.empty[A, B] else this

    override protected def filter0(p: ((A, B)) => Boolean, negate: Boolean, level: Int, buffer: Array[HashMap[A, B@uV]], offset0: Int): HashMap[A, B] =
      if (negate ^ p(ensurePair)) this else null

    override def iterator: Iterator[(A, B)] = Iterator(ensurePair)
    override def foreach[U](f: ((A, B)) => U): Unit = f(ensurePair)
    override private[immutable] def foreachEntry[U](f: (A, B) => U): Unit = f(key, value)
    // this method may be called multiple times in a multi-threaded environment, but that's ok
    private[HashMap] def ensurePair: (A, B) = if (kv ne null) kv else {
      kv = (key, value); kv
    }

    protected override def merge0[B1 >: B](that: HashMap[A, B1], level: Int, merger: Merger[A, B1]): HashMap[A, B1] = {
      that match {
        case hm1: HashMap1[A, B1] =>
          if ((this eq hm1) && merger.retainIdentical) this
          else if (this.hash == hm1.hash && this.key == hm1.key)
            if (merger eq HashMap.defaultMerger) this
            else if (merger eq HashMap.defaultMerger.invert) hm1
            else this.updated0(hm1.key, hm1.hash, level, hm1.value, hm1.kv, merger)
          else this.updated0(hm1.key, hm1.hash, level, hm1.value, hm1.kv, merger)
        case _ =>
          that.updated0(key, hash, level, value, kv, merger.invert)
      }
    }

    override def equals(that: Any): Boolean = {
      that match {
        case hm: HashMap1[_, _] =>
          (this eq hm) ||
            (hm.hash == hash && hm.key == key && hm.value == value)
        case _: HashMap[_, _] =>
          false
        case _ =>
          super.equals(that)
      }
    }

    protected override def transformImpl[W](f: (A, B) => W): HashMap[A, W] = {
      val value1 = f(key, value)
      if (value1.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) this.asInstanceOf[HashMap1[A, W]]
      else new HashMap1(key, hash, value1, null)
    }
  }

  private[collection] class HashMapCollision1[A, +B](private[collection] val hash: Int, val kvs: ListMap[A, B @uV])
          extends HashMap[A, B @uV] {
    // assert(kvs.size > 1)

    override def size = kvs.size

    override def get0(key: A, hash: Int, level: Int): Option[B] =
      if (hash == this.hash) kvs.get(key) else None

    override protected def contains0(key: A, hash: Int, level: Int): Boolean =
      hash == this.hash && kvs.contains(key)

    private[collection] override def updated0[B1 >: B](key: A, hash: Int, level: Int, value: B1, kv: (A, B1), merger: Merger[A, B1]): HashMap[A, B1] =
      if (hash == this.hash) {
        if ((merger eq null) || !kvs.contains(key)) new HashMapCollision1(hash, kvs.updated(key, value))
        else new HashMapCollision1(hash, kvs + merger((key, kvs(key)), kv))
      } else {
        val that = new HashMap1(key, hash, value, kv)
        makeHashTrieMap(this.hash, this, hash, that, level, size + 1)
      }

    override def removed0(key: A, hash: Int, level: Int): HashMap[A, B] =
      if (hash == this.hash) {
        val kvs1 = kvs - key
        kvs1.size match {
          case 0 =>
            HashMap.empty[A,B]
          case 1 =>
            val kv = kvs1.head
            new HashMap1(kv._1,hash,kv._2,kv)
          case x if x == kvs.size =>
            this
          case _ =>
            new HashMapCollision1(hash, kvs1)
        }
      } else this

    override protected def filter0(p: ((A, B)) => Boolean, negate: Boolean, level: Int, buffer: Array[HashMap[A, B @uV]], offset0: Int): HashMap[A, B] = {
      val kvs1 = if(negate) kvs.filterNot(p) else kvs.filter(p)
      kvs1.size match {
        case 0 =>
          null
        case 1 =>
          val kv@(k,v) = kvs1.head
          new HashMap1(k, hash, v, kv)
        case x if x == kvs.size =>
          this
        case _ =>
          new HashMapCollision1(hash, kvs1)
      }
    }

    override def iterator: Iterator[(A,B)] = kvs.iterator
    override def foreach[U](f: ((A, B)) => U): Unit = kvs.foreach(f)
    override private[immutable] def foreachEntry[U](f: (A, B) => U): Unit = kvs.foreachEntry(f)
    override def split: Seq[HashMap[A, B]] = {
      val (x, y) = kvs.splitAt(kvs.size / 2)
      def newhm(lm: ListMap[A, B @uV]) = {
        if (lm.size > 1) new HashMapCollision1(hash, lm)
        else new HashMap1(lm.head._1, hash, lm.head._2, lm.head)
      }
      List(newhm(x), newhm(y))
    }
    protected override def merge0[B1 >: B](that: HashMap[A, B1], level: Int, merger: Merger[A, B1]): HashMap[A, B1] = {
      that match {
        case hm: HashTrieMap[A, B1] =>
          //we ill get better performance and structural sharing by merging out one hashcode
          //into something that has by definition more that one hashcode
          hm.merge0(this, level, merger.invert)
        case h1: HashMap1[A, B1] =>
          if (h1.hash != hash) makeHashTrieMap(hash, this, h1.hash, h1, level, size + 1)
          else updated0(h1.key, h1.hash, level, h1.value, h1.kv, merger)
        case c: HashMapCollision1[A, B1] =>
          if (c.hash != hash) makeHashTrieMap(hash, this, c.hash, c, level, c.size + size)
          else if (merger.retainIdentical && (c eq this)) this
          else if ((merger eq defaultMerger) || (merger eq defaultMerger.invert)) {
            val newkvs = if (merger eq defaultMerger) c.kvs ++ this.kvs else this.kvs ++ c.kvs
            if (newkvs eq kvs) this
            else if (newkvs eq c.kvs) c
            else new HashMapCollision1(hash, newkvs)
          } else {
            var result: HashMap[A, B1] = null
            if (size >= c.size) {
              result = this
              for (p <- c.kvs) result = result.updated0(p._1, hash, level, p._2, p, merger)
            } else {
              result = c
              for (p <- kvs) result = result.updated0(p._1, hash, level, p._2, p, merger.invert)
            }
            result
          }
        case _ if that eq EmptyHashMap => this
      }
    }

    override def equals(that: Any): Boolean = {
      that match {
        case hm: HashMapCollision1[_,_] =>
          (this eq hm) ||
            (hm.hash == hash && hm.kvs == kvs)
        case _: HashMap[_, _] =>
          false
        case _ =>
          super.equals(that)
      }
    }

    protected override def transformImpl[W](f: (A, B) => W): HashMap[A, W] = {
      new HashMapCollision1[A, W](hash, kvs transform f)
    }
  }

  @deprecatedInheritance("This class will be made final in a future release.", "2.12.2")
  class HashTrieMap[A, +B](
    private[collection] val bitmap: Int,
    private[collection] val elems: Array[HashMap[A, B @uV]],
    override val size: Int
  ) extends HashMap[A, B @uV] {

    // assert(Integer.bitCount(bitmap) == elems.length)
    // assert(elems.length > 1 || (elems.length == 1 && elems(0).isInstanceOf[HashTrieMap[_,_]]))

    override def get0(key: A, hash: Int, level: Int): Option[B] = {
      // Note: this code is duplicated with `contains0`
      val index = (hash >>> level) & 0x1f
      if (bitmap == - 1) {
        elems(index).get0(key, hash, level + 5)
      } else {
        val mask = (1 << index)
        if ((bitmap & mask) != 0) {
          val offset = Integer.bitCount(bitmap & (mask - 1))
          elems(offset).get0(key, hash, level + 5)
        } else {
          None
        }
      }
    }

    override protected def contains0(key: A, hash: Int, level: Int): Boolean = {
      // Note: this code is duplicated from `get0`
      val index = (hash >>> level) & 0x1f
      if (bitmap == - 1) {
        elems(index).contains0(key, hash, level + 5)
      } else {
        val mask = (1 << index)
        if ((bitmap & mask) != 0) {
          val offset = Integer.bitCount(bitmap & (mask - 1))
          elems(offset).contains0(key, hash, level + 5)
        } else {
          false
        }
      }
    }

    private[collection] override def updated0[B1 >: B](key: A, hash: Int, level: Int, value: B1, kv: (A, B1), merger: Merger[A, B1]): HashMap[A, B1] = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      val offset = Integer.bitCount(bitmap & (mask - 1))
      if ((bitmap & mask) != 0) {
        val sub = elems(offset)
        val subNew = sub.updated0(key, hash, level + 5, value, kv, merger)
        if(subNew eq sub) this else {
          val elemsNew = new Array[HashMap[A,B1]](elems.length)
          System.arraycopy(elems, 0, elemsNew, 0, elems.length)
          elemsNew(offset) = subNew
          new HashTrieMap(bitmap, elemsNew, size + (subNew.size - sub.size))
        }
      } else {
        val elemsNew = new Array[HashMap[A,B1]](elems.length + 1)
        System.arraycopy(elems, 0, elemsNew, 0, offset)
        elemsNew(offset) = new HashMap1(key, hash, value, kv)
        System.arraycopy(elems, offset, elemsNew, offset + 1, elems.length - offset)
        new HashTrieMap(bitmap | mask, elemsNew, size + 1)
      }
    }

    override def removed0(key: A, hash: Int, level: Int): HashMap[A, B] = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      val offset = Integer.bitCount(bitmap & (mask - 1))
      if ((bitmap & mask) != 0) {
        val sub = elems(offset)
        val subNew = sub.removed0(key, hash, level + 5)
        if (subNew eq sub) this
        else if (subNew.isEmpty) {
          val bitmapNew = bitmap ^ mask
          if (bitmapNew != 0) {
            val elemsNew = new Array[HashMap[A,B]](elems.length - 1)
            System.arraycopy(elems, 0, elemsNew, 0, offset)
            System.arraycopy(elems, offset + 1, elemsNew, offset, elems.length - offset - 1)
            val sizeNew = size - sub.size
            // if we have only one child, which is not a HashTrieSet but a self-contained set like
            // HashSet1 or HashSetCollision1, return the child instead
            if (elemsNew.length == 1 && !elemsNew(0).isInstanceOf[HashTrieMap[_,_]])
              elemsNew(0)
            else
              new HashTrieMap(bitmapNew, elemsNew, sizeNew)
          } else
            HashMap.empty[A,B]
        } else if(elems.length == 1 && !subNew.isInstanceOf[HashTrieMap[_,_]]) {
          subNew
        } else {
          val elemsNew = new Array[HashMap[A,B]](elems.length)
          System.arraycopy(elems, 0, elemsNew, 0, elems.length)
          elemsNew(offset) = subNew
          val sizeNew = size + (subNew.size - sub.size)
          new HashTrieMap(bitmap, elemsNew, sizeNew)
        }
      } else {
        this
      }
    }

    override protected def filter0(p: ((A, B)) => Boolean, negate: Boolean, level: Int, buffer: Array[HashMap[A, B @uV]], offset0: Int): HashMap[A, B] = {
      // current offset
      var offset = offset0
      // result size
      var rs = 0
      // bitmap for kept elems
      var kept = 0
      // loop over all elements
      var i = 0
      while (i < elems.length) {
        val result = elems(i).filter0(p, negate, level + 5, buffer, offset)
        if (result ne null) {
          buffer(offset) = result
          offset += 1
          // add the result size
          rs += result.size
          // mark the bit i as kept
          kept |= (1 << i)
        }
        i += 1
      }
      if (offset == offset0) {
        // empty
        null
      } else if (rs == size) {
        // unchanged
        this
      } else if (offset == offset0 + 1 && !buffer(offset0).isInstanceOf[HashTrieMap[A, B]]) {
        // leaf
        buffer(offset0)
      } else {
        // we have to return a HashTrieMap
        val length = offset - offset0
        val elems1 = new Array[HashMap[A, B]](length)
        System.arraycopy(buffer, offset0, elems1, 0, length)
        val bitmap1 = if (length == elems.length) {
          // we can reuse the original bitmap
          bitmap
        } else {
          // calculate new bitmap by keeping just bits in the kept bitmask
          keepBits(bitmap, kept)
        }
        new HashTrieMap(bitmap1, elems1, rs)
      }
    }

    override def iterator: Iterator[(A, B)] = new TrieIterator[(A, B)](elems.asInstanceOf[Array[Iterable[(A, B)]]]) {
      final override def getElem(cc: AnyRef): (A, B) = cc.asInstanceOf[HashMap1[A, B]].ensurePair
    }

    override def foreach[U](f: ((A, B)) => U): Unit = {
      var i = 0
      while (i < elems.length) {
        elems(i).foreach(f)
        i += 1
      }
    }
    override private[immutable] def foreachEntry[U](f: (A, B) => U): Unit = {
      var i = 0
      while (i < elems.length) {
        elems(i).foreachEntry(f)
        i += 1
      }
    }

    private def posOf(n: Int, bm: Int) = {
      var left = n
      var i = -1
      var b = bm
      while (left >= 0) {
        i += 1
        if ((b & 1) != 0) left -= 1
        b = b >>> 1
      }
      i
    }

    override def split: Seq[HashMap[A, B]] = if (size == 1) Seq(this) else {
      val nodesize = Integer.bitCount(bitmap)
      if (nodesize > 1) {
        val splitpoint = nodesize / 2
        val bitsplitpoint = posOf(nodesize / 2, bitmap)
        val bm1 = bitmap & (-1 << bitsplitpoint)
        val bm2 = bitmap & (-1 >>> (32 - bitsplitpoint))

        val (e1, e2) = elems.splitAt(splitpoint)
        val hm1 = new HashTrieMap(bm1, e1, e1.foldLeft(0)(_ + _.size))
        val hm2 = new HashTrieMap(bm2, e2, e2.foldLeft(0)(_ + _.size))

        List(hm1, hm2)
      } else elems(0).split
    }

    protected[HashMap] override def merge0[B1 >: B](that: HashMap[A, B1], level: Int, merger: Merger[A, B1]): HashMap[A, B1] = that match {
      case hm: HashMap1[A, B1] =>
        this.updated0(hm.key, hm.hash, level, hm.value.asInstanceOf[B1], hm.kv, merger)
      case that: HashTrieMap[A, B1] =>
        def mergeMaybeSubset(larger: HashTrieMap[A, B1], smaller: HashTrieMap[A, B1], merger: Merger[A, B1]):HashTrieMap[A, B1] = {
          var resultElems: Array[HashMap[A, B1]] = null
          var ai = 0
          var bi = 0
          var abm = larger.bitmap
          var bbm = smaller.bitmap
          val a = larger.elems
          val b = smaller.elems

          //larger has all the bits or smaller, and if they have the same bits, is at least the bigger
          //so we try to merge `smaller`into `larger`and hope that `larger is a superset

          //the additional size in the results, so the eventual size of the result is larger.size + additionalSize
          var additionalSize = 0

          // could be lsb = Integer.lowestOneBit(abm)
          //but is this faster!!
          // keep fastest in step with adjustments in the loop
          //we know abm contains all of the bits in bbm, we only loop through bbm
          //bsb is the next lowest bit in smaller
          var bsb = bbm ^ (bbm & (bbm - 1))
          while (bsb != 0) {
            val skippedBitsInA = abm & (bsb - 1)
            ai += Integer.bitCount(skippedBitsInA)
            abm ^= skippedBitsInA
            val aai = a(ai)
            val bbi = b(bi)

            val result = if ((aai eq bbi) && merger.retainIdentical) aai
            else aai.merge0(bbi, level + 5, merger)
            if (result ne aai) {
              if (resultElems eq null)
                resultElems = a.clone()
              additionalSize += result.size - aai.size
              //assert (result.size > aai.size)
              resultElems(ai) = result
            }
            abm ^= bsb
            bbm ^= bsb
            bsb = bbm ^ (bbm & (bbm - 1))

            ai += 1
            bi += 1
          }
          // we don't have to check whether the result is a leaf, since union will only make the set larger
          // and this is not a leaf to begin with.
          if (resultElems eq null) larger // happy days - no change
          else new HashTrieMap(larger.bitmap, resultElems, larger.size + additionalSize)
        }
        def mergeDistinct() : HashMap[A,B1] = {
          // the maps are distinct, so its a bit simpler to combine
          // and we can avoid all of the quite expensive size calls on the children

          var ai = 0
          var bi = 0
          var offset = 0
          val abm = this.bitmap
          val bbm = that.bitmap
          val a = this.elems
          val b = that.elems
          var allBits = abm | bbm

          val resultElems = new Array[HashMap[A, B1]](Integer.bitCount(allBits))
          // could be lsb = Integer.lowestOneBit(abm)
          //but is this faster!!
          // keep fastest in step with adjustments in the loop
          // lowest remaining bit
          var lsb = allBits ^ (allBits & (allBits - 1))

          while (lsb != 0) {
            if ((lsb & abm) != 0) {
              resultElems(offset) = a(ai)
              ai += 1
            } else {
              resultElems(offset) = b(bi)
              bi += 1
            }
            offset += 1
            allBits ^= lsb
            lsb = allBits ^ (allBits & (allBits - 1))
          }
          // we don't have to check whether the result is a leaf, since merge will only make the maps larger
          // and this is not a leaf to begin with.
          new HashTrieMap[A, B1](allBits, resultElems, this.size + that.size)
    }
        def mergeCommon(): HashTrieMap[A, B1] = {
          var ai = 0
          var bi = 0
          val abm = this.bitmap
          val bbm = that.bitmap
          val a = this.elems
          val b = that.elems
          var allBits = abm | bbm
          val resultElems = new Array[HashMap[A, B1]](Integer.bitCount(allBits))

          //output index
          var offset = 0

          // the size of the results so far
          var rs = 0

          // could be alsb = Integer.lowestOneBit(abm)
          //but is this faster!!
          // keep fastest in step with adjustments in the loop
          // lowest remaining bit
          var lsb = allBits ^ (allBits & (allBits - 1))

          var result: HashMap[A, B1] = null
          // loop as long as there are bits left in either abm or bbm
          while (lsb != 0) {
            if ((lsb & abm) != 0) {
              if ((lsb & bbm) != 0) {
                // lsb is in a and b, so combine
                val aai = a(ai)
                val bbi = b(bi)

                result = if ((aai eq bbi) && merger.retainIdentical) aai
                else aai.merge0(bbi, level + 5, merger)
                ai += 1
                bi += 1
              } else {
                // lsb is in a
                result = a(ai)
                ai += 1
              }
            } else {
              // lsb is in b
              result = b(bi)
              bi += 1
            }
            // update lsb
            allBits ^= lsb
            lsb = allBits ^ (allBits & (allBits - 1))

            resultElems(offset) = result
            rs += result.size
            offset += 1
          }
          // we don't have to check whether the result is a leaf, since union will only make the set larger
          // and this is not a leaf to begin with.
          new HashTrieMap(this.bitmap | that.bitmap, resultElems, rs)

        }

        // if we have a subset/superset relationship, then we can merge and not allocate if thats a real subset
        // we check on that relationship based on the bitssets, and if the bitsets are the same than we look at the size
        // to work out the subset vs the superset
        // a superset here is a trie that has all the bits of the other and is possible to be a superset
        //
        // if the bits are distinct we can skip some processing so we have a path for that
        // otherwise the general case

        val abm = this.bitmap
        val bbm = that.bitmap
        val allBits = abm | bbm

        if ((this eq that) && merger.retainIdentical) this
        else if (allBits == abm && (allBits != bbm || this.size >= that.size)) mergeMaybeSubset(this, that, merger)
        else if (allBits == bbm) mergeMaybeSubset(that, this, merger.invert)
        else if ((abm & bbm) == 0) mergeDistinct()
        else mergeCommon()

      case hm: HashMapCollision1[_, _] =>
        val index = (hm.hash >>> level) & 0x1f
        val mask = (1 << index)
        val offset = Integer.bitCount(bitmap & (mask - 1))
        if ((bitmap & mask) != 0) {
          val sub = elems(offset)
          val subNew = sub.merge0(hm, level + 5, merger)
          if(subNew eq sub) this else {
            val elemsNew = elems.clone().asInstanceOf[Array[HashMap[A,B1]]]
            // its just a little faster than new Array[HashMap[A,B1]](elems.length); System.arraycopy(elems, 0, elemsNew, 0, elems.length)
            elemsNew(offset) = subNew
            new HashTrieMap(bitmap, elemsNew, size + (subNew.size - sub.size))
          }
        } else {
          val elemsNew = new Array[HashMap[A,B1]](elems.length + 1)
          System.arraycopy(elems, 0, elemsNew, 0, offset)
          elemsNew(offset) = hm
          System.arraycopy(elems, offset, elemsNew, offset + 1, elems.length - offset)
          new HashTrieMap(bitmap | mask, elemsNew, size + hm.size)
        }
      case _ if that eq EmptyHashMap => this
      case _ => sys.error("section supposed to be unreachable.")
    }

    override def equals(that: Any): Boolean = {
      that match {
        case hm: HashTrieMap[_, _] =>
          (this eq hm) || {
            this.bitmap == hm.bitmap &&
              this.size == hm.size &&
              ju.Arrays.equals(this.elems.asInstanceOf[Array[AnyRef]], hm.elems.asInstanceOf[Array[AnyRef]])
          }
        case _: HashMap[_, _] =>
          false
        case _ =>
          super.equals(that)
      }
    }

    protected override def transformImpl[W](f: (A, B) => W): HashMap[A, W] = {
      val elems1 = new Array[HashMap[A, W]](elems.length)
      var i = 0
      while (i < elems.length) {
        val elem = elems(i)
        if (elem ne null) {
          val elem1 = elem.transformImpl(f)
          elems1(i) = elem1
        }
        i += 1
      }
      new HashTrieMap[A, W](bitmap, elems1, size)
    }
  }

  /**
   * Calculates the maximum buffer size given the maximum possible total size of the trie-based collection
   * @param size the maximum size of the collection to be generated
   * @return the maximum buffer size
   */
  @inline private def bufferSize(size: Int): Int = (size + 6) min (32 * 7)

  /**
   * In many internal operations the empty map is represented as null for performance reasons. This method converts
   * null to the empty map for use in public methods
   */
  @inline private def nullToEmpty[A, B](m: HashMap[A, B]): HashMap[A, B] = if (m eq null) empty[A, B] else m

  /**
   * Utility method to keep a subset of all bits in a given bitmap
   *
   * Example
   *    bitmap (binary): 00000001000000010000000100000001
   *    keep (binary):                               1010
   *    result (binary): 00000001000000000000000100000000
   *
   * @param bitmap the bitmap
   * @param keep a bitmask containing which bits to keep
   * @return the original bitmap with all bits where keep is not 1 set to 0
   */
  private def keepBits(bitmap: Int, keep: Int): Int = {
    var result = 0
    var current = bitmap
    var kept = keep
    while (kept != 0) {
      // lowest remaining bit in current
      val lsb = current ^ (current & (current - 1))
      if ((kept & 1) != 0) {
        // mark bit in result bitmap
        result |= lsb
      }
      // clear lowest remaining one bit in abm
      current &= ~lsb
      // look at the next kept bit
      kept >>>= 1
    }
    result
  }

  @SerialVersionUID(2L)
  private class SerializationProxy[A,B](@transient private var orig: HashMap[A, B]) extends Serializable {
    private def writeObject(out: java.io.ObjectOutputStream) {
      val s = orig.size
      out.writeInt(s)
      for ((k,v) <- orig) {
        out.writeObject(k)
        out.writeObject(v)
      }
    }

    private def readObject(in: java.io.ObjectInputStream) {
      orig = empty
      val s = in.readInt()
      for (i <- 0 until s) {
        val key = in.readObject().asInstanceOf[A]
        val value = in.readObject().asInstanceOf[B]
        orig = orig.updated(key, value)
      }
    }

    private def readResolve(): AnyRef = orig
  }
}
