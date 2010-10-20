package scala.collection.parallel.immutable







import scala.collection.parallel.ParMap
import scala.collection.parallel.ParMapLike
import scala.collection.parallel.Combiner
import scala.collection.parallel.ParIterableIterator
import scala.collection.parallel.EnvironmentPassingCombiner
import scala.collection.generic.ParMapFactory
import scala.collection.generic.CanCombineFrom
import scala.collection.generic.GenericParMapTemplate
import scala.collection.generic.GenericParMapCompanion
import scala.collection.immutable.HashMap






/** Parallel hash trie map.
 *
 *  @author prokopec
 */
class ParHashMap[K, +V] private[immutable] (private[this] val trie: HashMap[K, V])
extends ParMap[K, V]
   with GenericParMapTemplate[K, V, ParHashMap]
   with ParMapLike[K, V, ParHashMap[K, V], HashMap[K, V]]
{
self =>

  def this() = this(HashMap.empty[K, V])

  override def mapCompanion: GenericParMapCompanion[ParHashMap] = ParHashMap

  override def empty: ParHashMap[K, V] = new ParHashMap[K, V]

  def parallelIterator: ParIterableIterator[(K, V)] = new ParHashMapIterator(trie.iterator, trie.size) with SCPI

  def seq = trie

  def -(k: K) = new ParHashMap(trie - k)

  def +[U >: V](kv: (K, U)) = new ParHashMap(trie + kv)

  def get(k: K) = trie.get(k)

  override def size = trie.size

  protected override def reuse[S, That](oldc: Option[Combiner[S, That]], newc: Combiner[S, That]) = oldc match {
    case Some(old) => old
    case None => newc
  }

  type SCPI = SignalContextPassingIterator[ParHashMapIterator]

  class ParHashMapIterator(val triter: Iterator[(K, V)], val sz: Int)
  extends super.ParIterator {
  self: SignalContextPassingIterator[ParHashMapIterator] =>
    var i = 0
    def split: Seq[ParIterator] = if (remaining < 2) Seq(this) else triter match {
      case t: HashMap.TrieIterator[_, _] =>
        val previousRemaining = remaining
        val ((fst, fstlength), snd) = t.asInstanceOf[HashMap.TrieIterator[K, V]].split
        val sndlength = previousRemaining - fstlength
        Seq(
          new ParHashMapIterator(fst, fstlength) with SCPI,
          new ParHashMapIterator(snd, sndlength) with SCPI
        )
      case _ =>
        // iterator of the collision map case
        val buff = triter.toBuffer
        val (fp, sp) = buff.splitAt(buff.length / 2)
        Seq(fp, sp) map { b => new ParHashMapIterator(b.iterator, b.length) with SCPI }
    }
    def next: (K, V) = {
      i += 1
      triter.next
    }
    def hasNext: Boolean = {
      i < sz
    }
    def remaining = sz - i
  }

}


object ParHashMap extends ParMapFactory[ParHashMap] {
  def empty[K, V]: ParHashMap[K, V] = new ParHashMap[K, V]

  def newCombiner[K, V]: Combiner[(K, V), ParHashMap[K, V]] = HashMapCombiner[K, V]

  implicit def canBuildFrom[K, V]: CanCombineFrom[Coll, (K, V), ParHashMap[K, V]] = {
    new CanCombineFromMap[K, V]
  }

  def fromTrie[K, V](t: HashMap[K, V]) = new ParHashMap(t)

  var totalcombines = new java.util.concurrent.atomic.AtomicInteger(0)
}


private[immutable] trait HashMapCombiner[K, V]
extends Combiner[(K, V), ParHashMap[K, V]] {
self: EnvironmentPassingCombiner[(K, V), ParHashMap[K, V]] =>
  import HashMapCombiner._
  var heads = new Array[Unrolled[(K, V)]](rootsize)
  var lasts = new Array[Unrolled[(K, V)]](rootsize)
  var size: Int = 0

  def clear = {
    heads = new Array[Unrolled[(K, V)]](rootsize)
    lasts = new Array[Unrolled[(K, V)]](rootsize)
  }

  def +=(elem: (K, V)) = {
    size += 1
    val hc = elem._1.##
    val pos = hc & 0x1f
    if (lasts(pos) eq null) {
      // initialize bucket
      heads(pos) = new Unrolled[(K, V)]
      lasts(pos) = heads(pos)
    }
    // add to bucket
    lasts(pos) = lasts(pos).add(elem)
    this
  }

  def combine[N <: (K, V), NewTo >: ParHashMap[K, V]](other: Combiner[N, NewTo]): Combiner[N, NewTo] = if (this ne other) {
    // ParHashMap.totalcombines.incrementAndGet
    if (other.isInstanceOf[HashMapCombiner[_, _]]) {
      val that = other.asInstanceOf[HashMapCombiner[K, V]]
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

    executeAndWaitResult(new CreateTrie(buckets, root, 0, buckets.length))

    var bitmap = 0
    var i = 0
    while (i < rootsize) {
      if (heads(i) ne null) bitmap |= 1 << i
      i += 1
    }
    val sz = root.foldLeft(0)(_ + _.size)

    if (sz == 0) new ParHashMap[K, V]
    else if (sz == 1) new ParHashMap[K, V](root(0))
    else {
      val trie = new HashMap.HashTrieMap(bitmap, root, sz)
      new ParHashMap[K, V](trie)
    }
  }

  /* tasks */

  class CreateTrie(buckets: Array[Unrolled[(K, V)]], root: Array[HashMap[K, V]], offset: Int, howmany: Int) extends super.Task[Unit, CreateTrie] {
    var result = ()
    def leaf(prev: Option[Unit]) = {
      var i = offset
      val until = offset + howmany
      while (i < until) {
        root(i) = createTrie(buckets(i))
        i += 1
      }
    }
    private def createTrie(elems: Unrolled[(K, V)]): HashMap[K, V] = {
      var trie = new HashMap[K, V]

      var unrolled = elems
      var i = 0
      while (unrolled ne null) {
        val chunkarr = unrolled.array
        val chunksz = unrolled.size
        while (i < chunksz) {
          val kv = chunkarr(i)
          val hc = kv._1.##
          trie = trie.updated0(kv._1, hc, rootbits, kv._2, kv, null)
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


object HashMapCombiner {
  def apply[K, V] = new HashMapCombiner[K, V] with EnvironmentPassingCombiner[(K, V), ParHashMap[K, V]] {}

  private[immutable] val rootbits = 5
  private[immutable] val rootsize = 1 << 5
}

















