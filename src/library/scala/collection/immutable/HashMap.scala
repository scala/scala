/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package immutable

import generic._
import annotation.unchecked.{ uncheckedVariance=> uV }
import parallel.immutable.ParHashMap

/** This class implements immutable maps using a hash trie.
 *
 *  '''Note:''' The builder of this hash map may return specialized representations for small maps.
 *
 *  @tparam A      the type of the keys contained in this hash map.
 *  @tparam B      the type of the values associated with the keys.
 *
 *  @author  Martin Odersky
 *  @author  Tiark Rompf
 *  @version 2.8
 *  @since   2.3
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#hash_tries "Scala's Collection Library overview"]]
 *  section on `Hash Tries` for more information.
 *  @define Coll `immutable.HashMap`
 *  @define coll immutable hash map
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
@SerialVersionUID(2L)
class HashMap[A, +B] extends AbstractMap[A, B]
                        with Map[A, B]
                        with MapLike[A, B, HashMap[A, B]]
                        with Serializable
                        with CustomParallelizable[(A, B), ParHashMap[A, B]]
{
  override def size: Int = 0

  override def empty = HashMap.empty[A, B]

  def iterator: Iterator[(A,B)] = Iterator.empty

  override def foreach[U](f: ((A, B)) =>  U): Unit = { }

  def get(key: A): Option[B] =
    get0(key, computeHash(key), 0)

  override def updated [B1 >: B] (key: A, value: B1): HashMap[A, B1] =
    updated0(key, computeHash(key), 0, value, null, null)

  override def + [B1 >: B] (kv: (A, B1)): HashMap[A, B1] =
    updated0(kv._1, computeHash(kv._1), 0, kv._2, kv, null)

  override def + [B1 >: B] (elem1: (A, B1), elem2: (A, B1), elems: (A, B1) *): HashMap[A, B1] =
    this + elem1 + elem2 ++ elems
    // TODO: optimize (might be able to use mutable updates)

  def - (key: A): HashMap[A, B] =
    removed0(key, computeHash(key), 0)

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

  private[collection] def updated0[B1 >: B](key: A, hash: Int, level: Int, value: B1, kv: (A, B1), merger: Merger[A, B1]): HashMap[A, B1] =
    new HashMap.HashMap1(key, hash, value, kv)

  protected def removed0(key: A, hash: Int, level: Int): HashMap[A, B] = this

  protected def writeReplace(): AnyRef = new HashMap.SerializationProxy(this)

  def split: Seq[HashMap[A, B]] = Seq(this)

  @deprecated("Use the `merged` method instead.", "2.10.0")
  def merge[B1 >: B](that: HashMap[A, B1], mergef: MergeFunction[A, B1] = null): HashMap[A, B1] = merge0(that, 0, liftMerger(mergef))
  
  /** Creates a new map which is the merge of this and the argument hash map.
   *  
   *  Uses the specified collision resolution function if two keys are the same.
   *  The collision resolution function will always take the first argument from
   *  `this` hash map and the second from `that`.
   *  
   *  The `merged` method is on average more performant than doing a traversal and reconstructing a
   *  new immutable hash map from scratch, or `++`.
   *  
   *  @tparam B1      the value type of the other hash map
   *  @param that     the other hash map
   *  @param mergef   the merge function or null if the first key-value pair is to be picked
   */
  def merged[B1 >: B](that: HashMap[A, B1])(mergef: MergeFunction[A, B1]): HashMap[A, B1] = merge0(that, 0, liftMerger(mergef))
  
  protected def merge0[B1 >: B](that: HashMap[A, B1], level: Int, merger: Merger[A, B1]): HashMap[A, B1] = that

  override def par = ParHashMap.fromTrie(this)

}

/** $factoryInfo
 *  @define Coll `immutable.HashMap`
 *  @define coll immutable hash map
 *
 *  @author  Tiark Rompf
 *  @since   2.3
 */
object HashMap extends ImmutableMapFactory[HashMap] with BitOperations.Int {
  
  private abstract class Merger[A, B] {
    def apply(kv1: (A, B), kv2: (A, B)): (A, B)
    def invert: Merger[A, B]
  }

  private type MergeFunction[A1, B1] = ((A1, B1), (A1, B1)) => (A1, B1)

  private def liftMerger[A1, B1](mergef: MergeFunction[A1, B1]): Merger[A1, B1] =
    if (mergef == null) defaultMerger.asInstanceOf[Merger[A1, B1]] else liftMerger0(mergef)

  private[this] val defaultMerger : Merger[Any, Any] = liftMerger0((a,b) => a)

  private[this] def liftMerger0[A1, B1](mergef: MergeFunction[A1, B1]): Merger[A1, B1] = new Merger[A1, B1] {
    self =>
    def apply(kv1: (A1, B1), kv2: (A1, B1)): (A1, B1) = mergef(kv1, kv2)
    val invert: Merger[A1, B1] = new Merger[A1, B1] {
      def apply(kv1: (A1, B1), kv2: (A1, B1)): (A1, B1) = mergef(kv2, kv1)
      def invert: Merger[A1, B1] = self
    }
  }
  
  /** $mapCanBuildFromInfo */
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), HashMap[A, B]] = new MapCanBuildFrom[A, B]
  def empty[A, B]: HashMap[A, B] = EmptyHashMap.asInstanceOf[HashMap[A, B]]

  private object EmptyHashMap extends HashMap[Any, Nothing] { }

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

  // TODO: add HashMap2, HashMap3, ...

  class HashMap1[A,+B](private[collection] val key: A, private[collection] val hash: Int, private[collection] val value: (B @uV), private[collection] var kv: (A,B @uV)) extends HashMap[A,B] {
    override def size = 1

    private[collection] def getKey = key
    private[collection] def getHash = hash
    private[collection] def computeHashFor(k: A) = computeHash(k)

    override def get0(key: A, hash: Int, level: Int): Option[B] =
      if (hash == this.hash && key == this.key) Some(value) else None

    // override def updated0[B1 >: B](key: A, hash: Int, level: Int, value: B1, kv: (A, B1)): HashMap[A, B1] =
    //   if (hash == this.hash && key == this.key) new HashMap1(key, hash, value, kv)
    //   else {
    //     var thatindex = (hash >>> level) & 0x1f
    //     var thisindex = (this.hash >>> level) & 0x1f
    //     if (hash != this.hash) {
    //       --new HashTrieMap[A,B1](level+5, this, new HashMap1(key, hash, value, kv))
    //       val m = new HashTrieMap[A,B1](0,new Array[HashMap[A,B1]](0),0) // TODO: could save array alloc
    //       m.updated0(this.key, this.hash, level, this.value, this.kv).updated0(key, hash, level, value, kv)  TODO and it will
    //     } else {
    //        32-bit hash collision (rare, but not impossible)
    //       new HashMapCollision1(hash, ListMap.empty.updated(this.key,this.value).updated(key,value))
    //     }
    //   }

    override def updated0[B1 >: B](key: A, hash: Int, level: Int, value: B1, kv: (A, B1), merger: Merger[A, B1]): HashMap[A, B1] =
      if (hash == this.hash && key == this.key ) {
        if (merger eq null) {
          if (this.value.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) this
          else new HashMap1(key, hash, value, kv)
        } else {
          val nkv = merger(this.kv, kv)
          new HashMap1(nkv._1, hash, nkv._2, nkv)
        }
      } else {
        if (hash != this.hash) {
          // they have different hashes, but may collide at this level - find a level at which they don't
          val that = new HashMap1[A, B1](key, hash, value, kv)
          makeHashTrieMap[A,B1](this.hash, this, hash, that, level, 2)
        } else {
          // 32-bit hash collision (rare, but not impossible)
          new HashMapCollision1(hash, ListMap.empty.updated(this.key,this.value).updated(key,value))
        }
      }

    override def removed0(key: A, hash: Int, level: Int): HashMap[A, B] =
      if (hash == this.hash && key == this.key) HashMap.empty[A,B] else this

    override def iterator: Iterator[(A,B)] = Iterator(ensurePair)
    override def foreach[U](f: ((A, B)) => U): Unit = f(ensurePair)
    // this method may be called multiple times in a multithreaded environment, but that's ok
    private[HashMap] def ensurePair: (A,B) = if (kv ne null) kv else { kv = (key, value); kv }
    protected override def merge0[B1 >: B](that: HashMap[A, B1], level: Int, merger: Merger[A, B1]): HashMap[A, B1] = {
      that.updated0(key, hash, level, value, kv, merger.invert)
    }
  }

  private[collection] class HashMapCollision1[A, +B](private[collection] val hash: Int, val kvs: ListMap[A, B @uV])
          extends HashMap[A, B @uV] {
    // assert(kvs.size > 1)

    override def size = kvs.size

    override def get0(key: A, hash: Int, level: Int): Option[B] =
      if (hash == this.hash) kvs.get(key) else None

    override def updated0[B1 >: B](key: A, hash: Int, level: Int, value: B1, kv: (A, B1), merger: Merger[A, B1]): HashMap[A, B1] =
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
        if (kvs1.isEmpty)
          HashMap.empty[A,B]
        else if(kvs1.tail.isEmpty) {
          val kv = kvs1.head
          new HashMap1[A,B](kv._1,hash,kv._2,kv)
        } else
          new HashMapCollision1(hash, kvs1)
      } else this

    override def iterator: Iterator[(A,B)] = kvs.iterator
    override def foreach[U](f: ((A, B)) => U): Unit = kvs.foreach(f)
    override def split: Seq[HashMap[A, B]] = {
      val (x, y) = kvs.splitAt(kvs.size / 2)
      def newhm(lm: ListMap[A, B @uV]) = new HashMapCollision1(hash, lm)
      List(newhm(x), newhm(y))
    }
    protected override def merge0[B1 >: B](that: HashMap[A, B1], level: Int, merger: Merger[A, B1]): HashMap[A, B1] = {
      // this can be made more efficient by passing the entire ListMap at once
      var m = that
      for (p <- kvs) m = m.updated0(p._1, this.hash, level, p._2, p, merger)
      m
    }
  }

  class HashTrieMap[A, +B](
    private[collection] val bitmap: Int,
    private[collection] val elems: Array[HashMap[A, B @uV]],
    private[collection] val size0: Int
  ) extends HashMap[A, B @uV] {

    // assert(Integer.bitCount(bitmap) == elems.length)
    // assert(elems.length > 1 || (elems.length == 1 && elems(0).isInstanceOf[HashTrieMap[_,_]]))

/*
    def this (level: Int, m1: HashMap1[A,B], m2: HashMap1[A,B]) = {
      this(((m1.hash >>> level) & 0x1f) | ((m2.hash >>> level) & 0x1f), {
        val idx1 = (m1.hash >>> level) & 0x1f
        val idx2 = (m2.hash >>> level) & 0x1f
        assert(idx1 != idx2, m1.hash + "==" + m2.hash + " at level " + level) // TODO
        val elems = new Array[HashMap[A,B]](2)
        if (idx1 < idx2) {
          elems(0) = m1
          elems(1) = m2
        } else {
          elems(0) = m2
          elems(1) = m1
        }
        elems
      }, 2)
    }
*/
    override def size = size0

    override def get0(key: A, hash: Int, level: Int): Option[B] = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      if (bitmap == - 1) {
        elems(index & 0x1f).get0(key, hash, level + 5)
      } else if ((bitmap & mask) != 0) {
        val offset = Integer.bitCount(bitmap & (mask-1))
        // TODO: might be worth checking if sub is HashTrieMap (-> monomorphic call site)
        elems(offset).get0(key, hash, level + 5)
      } else
        None
    }

    override def updated0[B1 >: B](key: A, hash: Int, level: Int, value: B1, kv: (A, B1), merger: Merger[A, B1]): HashMap[A, B1] = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      val offset = Integer.bitCount(bitmap & (mask-1))
      if ((bitmap & mask) != 0) {
        val sub = elems(offset)
        // TODO: might be worth checking if sub is HashTrieMap (-> monomorphic call site)
        val subNew = sub.updated0(key, hash, level + 5, value, kv, merger)
        if(subNew eq sub) this else {
          val elemsNew = new Array[HashMap[A,B1]](elems.length)
          Array.copy(elems, 0, elemsNew, 0, elems.length)
          elemsNew(offset) = subNew
          new HashTrieMap(bitmap, elemsNew, size + (subNew.size - sub.size))
        }
      } else {
        val elemsNew = new Array[HashMap[A,B1]](elems.length + 1)
        Array.copy(elems, 0, elemsNew, 0, offset)
        elemsNew(offset) = new HashMap1(key, hash, value, kv)
        Array.copy(elems, offset, elemsNew, offset + 1, elems.length - offset)
        new HashTrieMap(bitmap | mask, elemsNew, size + 1)
      }
    }

    override def removed0(key: A, hash: Int, level: Int): HashMap[A, B] = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      val offset = Integer.bitCount(bitmap & (mask-1))
      if ((bitmap & mask) != 0) {
        val sub = elems(offset)
        // TODO: might be worth checking if sub is HashTrieMap (-> monomorphic call site)
        val subNew = sub.removed0(key, hash, level + 5)
        if (subNew eq sub) this
        else if (subNew.isEmpty) {
          val bitmapNew = bitmap ^ mask
          if (bitmapNew != 0) {
            val elemsNew = new Array[HashMap[A,B]](elems.length - 1)
            Array.copy(elems, 0, elemsNew, 0, offset)
            Array.copy(elems, offset + 1, elemsNew, offset, elems.length - offset - 1)
            val sizeNew = size - sub.size
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
          Array.copy(elems, 0, elemsNew, 0, elems.length)
          elemsNew(offset) = subNew
          val sizeNew = size + (subNew.size - sub.size)
          new HashTrieMap(bitmap, elemsNew, sizeNew)
        }
      } else {
        this
      }
    }

    override def iterator: Iterator[(A, B)] = new TrieIterator[(A, B)](elems.asInstanceOf[Array[Iterable[(A, B)]]]) {
      final override def getElem(cc: AnyRef): (A, B) = cc.asInstanceOf[HashMap1[A, B]].ensurePair
    }

/*
def time(block: =>Unit) = { val t0 = System.nanoTime; block; println("elapsed: " + (System.nanoTime - t0)/1000000.0) }
var mOld = OldHashMap.empty[Int,Int]
var mNew = HashMap.empty[Int,Int]
time { for (i <- 0 until 100000) mOld = mOld.updated(i,i) }
time { for (i <- 0 until 100000) mOld = mOld.updated(i,i) }
time { for (i <- 0 until 100000) mOld = mOld.updated(i,i) }
time { for (i <- 0 until 100000) mNew = mNew.updated(i,i) }
time { for (i <- 0 until 100000) mNew = mNew.updated(i,i) }
time { for (i <- 0 until 100000) mNew = mNew.updated(i,i) }
time { mOld.iterator.foreach( p => ()) }
time { mOld.iterator.foreach( p => ()) }
time { mOld.iterator.foreach( p => ()) }
time { mNew.iterator.foreach( p => ()) }
time { mNew.iterator.foreach( p => ()) }
time { mNew.iterator.foreach( p => ()) }
*/

    override def foreach[U](f: ((A, B)) =>  U): Unit = {
      var i = 0;
      while (i < elems.length) {
        elems(i).foreach(f)
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

    protected override def merge0[B1 >: B](that: HashMap[A, B1], level: Int, merger: Merger[A, B1]): HashMap[A, B1] = that match {
      case hm: HashMap1[_, _] =>
        this.updated0(hm.key, hm.hash, level, hm.value.asInstanceOf[B1], hm.kv, merger)
      case hm: HashTrieMap[_, _] =>
        val that = hm.asInstanceOf[HashTrieMap[A, B1]]
        val thiselems = this.elems
        val thatelems = that.elems
        var thisbm = this.bitmap
        var thatbm = that.bitmap

        // determine the necessary size for the array
        val subcount = Integer.bitCount(thisbm | thatbm)

        // construct a new array of appropriate size
        val merged = new Array[HashMap[A, B1]](subcount)

        // run through both bitmaps and add elements to it
        var i = 0
        var thisi = 0
        var thati = 0
        var totalelems = 0
        while (i < subcount) {
          val thislsb = thisbm ^ (thisbm & (thisbm - 1))
          val thatlsb = thatbm ^ (thatbm & (thatbm - 1))

          // collision
          if (thislsb == thatlsb) {
            val m = thiselems(thisi).merge0(thatelems(thati), level + 5, merger)
            totalelems += m.size
            merged(i) = m
            thisbm = thisbm & ~thislsb
            thatbm = thatbm & ~thatlsb
            thati += 1
            thisi += 1
          } else {
            // condition below is due to 2 things:
            // 1) no unsigned int compare on JVM
            // 2) 0 (no lsb) should always be greater in comparison
            val a = thislsb - 1
            val b = thatlsb - 1

            if (unsignedCompare(thislsb - 1, thatlsb - 1)) {
              val m = thiselems(thisi)
              totalelems += m.size
              merged(i) = m
              thisbm = thisbm & ~thislsb
              thisi += 1
            }
            else {
              val m = thatelems(thati)
              totalelems += m.size
              merged(i) = m
              thatbm = thatbm & ~thatlsb
              thati += 1
            }
          }
          i += 1
        }

        new HashTrieMap[A, B1](this.bitmap | that.bitmap, merged, totalelems)
      case hm: HashMapCollision1[_, _] => that.merge0(this, level, merger.invert)
      case hm: HashMap[_, _] => this
      case _ => sys.error("section supposed to be unreachable.")
    }
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
