package scala.collection.parallel.immutable







import scala.collection.parallel.ParallelMap
import scala.collection.parallel.ParallelMapLike
import scala.collection.parallel.Combiner
import scala.collection.parallel.EnvironmentPassingCombiner
import scala.collection.generic.ParallelMapFactory
import scala.collection.generic.CanCombineFrom
import scala.collection.generic.GenericParallelMapTemplate
import scala.collection.generic.GenericParallelMapCompanion
import scala.collection.immutable.HashMap






/** Parallel hash trie map.
 *
 *  @author prokopec
 */
class ParallelHashTrie[K, +V] private[immutable] (private[this] val trie: HashMap[K, V])
extends ParallelMap[K, V]
   with GenericParallelMapTemplate[K, V, ParallelHashTrie]
   with ParallelMapLike[K, V, ParallelHashTrie[K, V], HashMap[K, V]]
{
self =>

  def this() = this(HashMap.empty[K, V])

  override def mapCompanion: GenericParallelMapCompanion[ParallelHashTrie] = ParallelHashTrie

  override def empty: ParallelHashTrie[K, V] = new ParallelHashTrie[K, V]

  def parallelIterator = new ParallelHashTrieIterator(trie) with SCPI

  def seq = trie

  def -(k: K) = new ParallelHashTrie(trie - k)

  def +[U >: V](kv: (K, U)) = new ParallelHashTrie(trie + kv)

  def get(k: K) = trie.get(k)

  override def size = trie.size

  protected override def reuse[S, That](oldc: Option[Combiner[S, That]], newc: Combiner[S, That]) = oldc match {
    case Some(old) => old
    case None => newc
  }

  type SCPI = SignalContextPassingIterator[ParallelHashTrieIterator]

  class ParallelHashTrieIterator(val ht: HashMap[K, V])
  extends super.ParallelIterator {
  self: SignalContextPassingIterator[ParallelHashTrieIterator] =>
    // println("created iterator " + ht)
    var i = 0
    lazy val triter = ht.iterator
    def split: Seq[ParallelIterator] = {
      // println("splitting " + ht + " into " + ht.split.map(new ParallelHashTrieIterator(_) with SCPI).map(_.toList))
      ht.split.map(new ParallelHashTrieIterator(_) with SCPI)
    }
    def next: (K, V) = {
      // println("taking next after " + i + ", in " + ht)
      i += 1
      triter.next
    }
    def hasNext: Boolean = {
      // println("hasNext: " + i + ", " + ht.size + ", " + ht)
      i < ht.size
    }
    def remaining = ht.size - i
  }

}


object ParallelHashTrie extends ParallelMapFactory[ParallelHashTrie] {
  def empty[K, V]: ParallelHashTrie[K, V] = new ParallelHashTrie[K, V]

  def newCombiner[K, V]: Combiner[(K, V), ParallelHashTrie[K, V]] = HashTrieCombiner[K, V]

  implicit def canBuildFrom[K, V]: CanCombineFrom[Coll, (K, V), ParallelHashTrie[K, V]] = {
    new CanCombineFromMap[K, V]
  }

  def fromTrie[K, V](t: HashMap[K, V]) = new ParallelHashTrie(t)

  var totalcombines = new java.util.concurrent.atomic.AtomicInteger(0)
}


trait HashTrieCombiner[K, V]
extends Combiner[(K, V), ParallelHashTrie[K, V]] {
self: EnvironmentPassingCombiner[(K, V), ParallelHashTrie[K, V]] =>
  import HashTrieCombiner._
  var heads = new Array[Unrolled[K, V]](rootsize)
  var lasts = new Array[Unrolled[K, V]](rootsize)
  var size: Int = 0

  def clear = {
    heads = new Array[Unrolled[K, V]](rootsize)
    lasts = new Array[Unrolled[K, V]](rootsize)
  }

  def +=(elem: (K, V)) = {
    size += 1
    val hc = elem._1.##
    val pos = hc & 0x1f
    if (lasts(pos) eq null) {
      // initialize bucket
      heads(pos) = new Unrolled[K, V]
      lasts(pos) = heads(pos)
    }
    // add to bucket
    lasts(pos) = lasts(pos).add(elem)
    this
  }

  def combine[N <: (K, V), NewTo >: ParallelHashTrie[K, V]](other: Combiner[N, NewTo]): Combiner[N, NewTo] = if (this ne other) {
    // ParallelHashTrie.totalcombines.incrementAndGet
    if (other.isInstanceOf[HashTrieCombiner[_, _]]) {
      val that = other.asInstanceOf[HashTrieCombiner[K, V]]
      var i = 0
      while (i < rootsize) {
        if (lasts(i) eq null) {
          heads(i) = that.heads(i)
          lasts(i) = that.lasts(i)
        } else {
          lasts(i).next = that.heads(i)
          if (that.lasts(i) ne null) lasts(i) = that.lasts(i)
        }
        i += 1
      }
      size = size + that.size
      this
    } else error("Unexpected combiner type.")
  } else this

  def result = {
    val buckets = heads.filter(_ != null)
    val root = new Array[HashMap[K, V]](buckets.length)

    executeAndWait(new CreateTrie(buckets, root, 0, buckets.length))

    var bitmap = 0
    var i = 0
    while (i < rootsize) {
      if (heads(i) ne null) bitmap |= 1 << i
      i += 1
    }
    val sz = root.foldLeft(0)(_ + _.size)

    if (sz == 0) new ParallelHashTrie[K, V]
    else if (sz == 1) new ParallelHashTrie[K, V](root(0))
    else {
      val trie = new HashMap.HashTrieMap(bitmap, root, sz)
      new ParallelHashTrie[K, V](trie)
    }
  }

  /* tasks */

  class CreateTrie(buckets: Array[Unrolled[K, V]], root: Array[HashMap[K, V]], offset: Int, howmany: Int) extends super.Task[Unit, CreateTrie] {
    var result = ()
    def leaf(prev: Option[Unit]) = {
      var i = offset
      val until = offset + howmany
      while (i < until) {
        root(i) = createTrie(buckets(i))
        i += 1
      }
    }
    private def createTrie(elems: Unrolled[K, V]): HashMap[K, V] = {
      var trie = new HashMap[K, V]

      var unrolled = elems
      var i = 0
      while (unrolled ne null) {
        val chunkarr = unrolled.array
        val chunksz = unrolled.size
        while (i < chunksz) {
          val kv = chunkarr(i)
          val hc = kv._1.##
          trie = trie.updated0(kv._1, hc, rootbits, kv._2, kv)
          i += 1
        }
        i = 0
        unrolled = unrolled.next
      }

      trie
    }
    def split = {
      val fp = howmany / 2
      List(new CreateTrie(buckets, root, offset, fp), new CreateTrie(buckets, root, offset + fp, howmany - fp))
    }
    def shouldSplitFurther = howmany > collection.parallel.thresholdFromSize(root.length, parallelismLevel)
  }

}


object HashTrieCombiner {
  def apply[K, V] = new HashTrieCombiner[K, V] with EnvironmentPassingCombiner[(K, V), ParallelHashTrie[K, V]] {}

  private[immutable] val rootbits = 5
  private[immutable] val rootsize = 1 << 5
  private[immutable] val unrolledsize = 16

  private[immutable] class Unrolled[K, V] {
    var size = 0
    var array = new Array[(K, V)](unrolledsize)
    var next: Unrolled[K, V] = null
    // adds and returns itself or the new unrolled if full
    def add(elem: (K, V)): Unrolled[K, V] = if (size < unrolledsize) {
      array(size) = elem
      size += 1
      this
    } else {
      next = new Unrolled[K, V]
      next.add(elem)
    }
    override def toString = "Unrolled(" + array.mkString(", ") + ")"
  }
}















