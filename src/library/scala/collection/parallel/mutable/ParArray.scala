/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala
package collection.parallel.mutable



import scala.collection.generic.GenericParTemplate
import scala.collection.generic.GenericCompanion
import scala.collection.generic.GenericParCompanion
import scala.collection.generic.CanCombineFrom
import scala.collection.generic.CanBuildFrom
import scala.collection.generic.ParFactory
import scala.collection.parallel.Combiner
import scala.collection.parallel.SeqSplitter
import scala.collection.parallel.ParSeqLike
import scala.collection.parallel.Task
import scala.collection.parallel.CHECK_RATE
import scala.collection.mutable.ArraySeq
import scala.collection.mutable.Builder
import scala.collection.GenTraversableOnce
import scala.reflect.ClassTag

/** Parallel sequence holding elements in a linear array.
 *
 *  `ParArray` is a parallel sequence with a predefined size. The size of the array
 *  cannot be changed after it's been created.
 *
 *  `ParArray` internally keeps an array containing the elements. This means that
 *  bulk operations based on traversal ensure fast access to elements. `ParArray` uses lazy builders that
 *  create the internal data array only after the size of the array is known. In the meantime, they keep
 *  the result set fragmented. The fragments
 *  are copied into the resulting data array in parallel using fast array copy operations once all the combiners
 *  are populated in parallel.
 *
 *  @tparam T        type of the elements in the array
 *
 *  @author Aleksandar Prokopec
 *  @since 2.9
 *  @see  [[http://docs.scala-lang.org/overviews/parallel-collections/concrete-parallel-collections.html#parallel_array Scala's Parallel Collections Library overview]]
 *  section on `ParArray` for more information.
 *
 *  @define Coll `ParArray`
 *  @define coll parallel array
 *
 */
@SerialVersionUID(1L)
class ParArray[T] private[mutable] (val arrayseq: ArraySeq[T])
extends ParSeq[T]
   with GenericParTemplate[T, ParArray]
   with ParSeqLike[T, ParArray[T], ArraySeq[T]]
   with Serializable
{
self =>

  @transient private var array: Array[Any] = arrayseq.array.asInstanceOf[Array[Any]]

  override def companion: GenericCompanion[ParArray] with GenericParCompanion[ParArray] = ParArray

  def this(sz: Int) = this {
    require(sz >= 0)
    new ArraySeq[T](sz)
  }

  def apply(i: Int) = array(i).asInstanceOf[T]

  def update(i: Int, elem: T) = array(i) = elem

  def length = arrayseq.length

  override def seq = arrayseq

  protected[parallel] def splitter: ParArrayIterator = {
    val pit = new ParArrayIterator
    pit
  }

  class ParArrayIterator(var i: Int = 0, val until: Int = length, val arr: Array[Any] = array)
  extends SeqSplitter[T] {
    def hasNext = i < until

    def next = {
      val elem = arr(i)
      i += 1
      elem.asInstanceOf[T]
    }

    def remaining = until - i

    def dup = new ParArrayIterator(i, until, arr)

    def psplit(sizesIncomplete: Int*): Seq[ParArrayIterator] = {
      var traversed = i
      val total = sizesIncomplete.reduceLeft(_ + _)
      val left = remaining
      val sizes = if (total >= left) sizesIncomplete else sizesIncomplete :+ (left - total)
      for (sz <- sizes) yield if (traversed < until) {
        val start = traversed
        val end = (traversed + sz) min until
        traversed = end
        new ParArrayIterator(start, end, arr)
      } else {
        new ParArrayIterator(traversed, traversed, arr)
      }
    }

    override def split: Seq[ParArrayIterator] = {
      val left = remaining
      if (left >= 2) {
        val splitpoint = left / 2
        val sq = Seq(
          new ParArrayIterator(i, i + splitpoint, arr),
          new ParArrayIterator(i + splitpoint, until, arr))
        i = until
        sq
      } else {
        Seq(this)
      }
    }

    override def toString = "ParArrayIterator(" + i + ", " + until + ")"

    /* overrides for efficiency */

    /* accessors */

    override def foreach[U](f: T => U) = {
      foreach_quick(f, arr, until, i)
      i = until
    }

    private def foreach_quick[U](f: T => U, a: Array[Any], ntil: Int, from: Int) = {
      var j = from
      while (j < ntil) {
        f(a(j).asInstanceOf[T])
        j += 1
      }
    }

    override def count(p: T => Boolean) = {
      val c = count_quick(p, arr, until, i)
      i = until
      c
    }

    private def count_quick(p: T => Boolean, a: Array[Any], ntil: Int, from: Int) = {
      var cnt = 0
      var j = from
      while (j < ntil) {
        if (p(a(j).asInstanceOf[T])) cnt += 1
        j += 1
      }
      cnt
    }

    override def foldLeft[S](z: S)(op: (S, T) => S): S = {
      val r = foldLeft_quick(arr, until, op, z)
      i = until
      r
    }

    private def foldLeft_quick[S](a: Array[Any], ntil: Int, op: (S, T) => S, z: S): S = {
      var j = i
      var sum = z
      while (j < ntil) {
        sum = op(sum, a(j).asInstanceOf[T])
        j += 1
      }
      sum
    }

    override def fold[U >: T](z: U)(op: (U, U) => U): U = foldLeft[U](z)(op)

    override def aggregate[S](z: =>S)(seqop: (S, T) => S, combop: (S, S) => S): S = foldLeft[S](z)(seqop)

    override def sum[U >: T](implicit num: Numeric[U]): U = {
      val s = sum_quick(num, arr, until, i, num.zero)
      i = until
      s
    }

    private def sum_quick[U >: T](num: Numeric[U], a: Array[Any], ntil: Int, from: Int, zero: U): U = {
      var j = from
      var sum = zero
      while (j < ntil) {
        sum = num.plus(sum, a(j).asInstanceOf[T])
        j += 1
      }
      sum
    }

    override def product[U >: T](implicit num: Numeric[U]): U = {
        val p = product_quick(num, arr, until, i, num.one)
        i = until
        p
    }

    private def product_quick[U >: T](num: Numeric[U], a: Array[Any], ntil: Int, from: Int, one: U): U = {
        var j = from
        var prod = one
        while (j < ntil) {
          prod = num.times(prod, a(j).asInstanceOf[T])
          j += 1
        }
        prod
    }

    override def forall(p: T => Boolean): Boolean = {
      if (isAborted) return false

      var all = true
      while (i < until) {
        val nextuntil = if (i + CHECK_RATE > until) until else i + CHECK_RATE

        all = forall_quick(p, array, nextuntil, i)
        if (all) i = nextuntil
        else {
          i = until
          abort()
        }

        if (isAborted) return false
      }
      all
    }

    // it's faster to use a separate small method
    private def forall_quick(p: T => Boolean, a: Array[Any], nextuntil: Int, start: Int): Boolean = {
      var j = start
      while (j < nextuntil) {
        if (p(a(j).asInstanceOf[T])) j += 1
        else return false
      }
      true
    }

    override def exists(p: T => Boolean): Boolean = {
      if (isAborted) return true

      var some = false
      while (i < until) {
        val nextuntil = if (i + CHECK_RATE > until) until else i + CHECK_RATE

        some = exists_quick(p, array, nextuntil, i)
        if (some) {
          i = until
          abort()
        } else i = nextuntil

        if (isAborted) return true
      }
      some
    }

    // faster to use separate small method
    private def exists_quick(p: T => Boolean, a: Array[Any], nextuntil: Int, start: Int): Boolean = {
      var j = start
      while (j < nextuntil) {
        if (p(a(j).asInstanceOf[T])) return true
        else j += 1
      }
      false
    }

    override def find(p: T => Boolean): Option[T] = {
      if (isAborted) return None

      var r: Option[T] = None
      while (i < until) {
        val nextuntil = if ((i + CHECK_RATE) < until) (i + CHECK_RATE) else until

        r = find_quick(p, array, nextuntil, i)

        if (r != None) {
          i = until
          abort()
        } else i = nextuntil

        if (isAborted) return r
      }
      r
    }

    private def find_quick(p: T => Boolean, a: Array[Any], nextuntil: Int, start: Int): Option[T] = {
      var j = start
      while (j < nextuntil) {
        val elem = a(j).asInstanceOf[T]
        if (p(elem)) return Some(elem)
        else j += 1
      }
      None
    }

    override def drop(n: Int): ParArrayIterator = {
      i += n
      this
    }

    override def copyToArray[U >: T](array: Array[U], from: Int, len: Int) {
      val totallen = (self.length - i) min len min (array.length - from)
      Array.copy(arr, i, array, from, totallen)
      i += totallen
    }

    override def prefixLength(pred: T => Boolean): Int = {
      val r = prefixLength_quick(pred, arr, until, i)
      i += r + 1
      r
    }

    private def prefixLength_quick(pred: T => Boolean, a: Array[Any], ntil: Int, startpos: Int): Int = {
      var j = startpos
      var endpos = ntil
      while (j < endpos) {
        if (pred(a(j).asInstanceOf[T])) j += 1
        else endpos = j
      }
      endpos - startpos
    }

    override def indexWhere(pred: T => Boolean): Int = {
      val r = indexWhere_quick(pred, arr, until, i)
      val ret = if (r != -1) r - i else r
      i = until
      ret
    }

    private def indexWhere_quick(pred: T => Boolean, a: Array[Any], ntil: Int, from: Int): Int = {
      var j = from
      var pos = -1
      while (j < ntil) {
        if (pred(a(j).asInstanceOf[T])) {
          pos = j
          j = ntil
        } else j += 1
      }
      pos
    }

    override def lastIndexWhere(pred: T => Boolean): Int = {
      val r = lastIndexWhere_quick(pred, arr, i, until)
      val ret = if (r != -1) r - i else r
      i = until
      ret
    }

    private def lastIndexWhere_quick(pred: T => Boolean, a: Array[Any], from: Int, ntil: Int): Int = {
      var pos = -1
      var j = ntil - 1
      while (j >= from) {
        if (pred(a(j).asInstanceOf[T])) {
          pos = j
          j = -1
        } else j -= 1
      }
      pos
    }

    override def sameElements(that: Iterator[_]): Boolean = {
      var same = true
      while (i < until && that.hasNext) {
        if (arr(i) != that.next) {
          i = until
          same = false
        }
        i += 1
      }
      same
    }

    /* transformers */

    override def map2combiner[S, That](f: T => S, cb: Combiner[S, That]): Combiner[S, That] = {
      //val cb = cbf(self.repr)
      cb.sizeHint(remaining)
      map2combiner_quick(f, arr, cb, until, i)
      i = until
      cb
    }

    private def map2combiner_quick[S, That](f: T => S, a: Array[Any], cb: Builder[S, That], ntil: Int, from: Int) {
      var j = from
      while (j < ntil) {
        cb += f(a(j).asInstanceOf[T])
        j += 1
      }
    }

    override def collect2combiner[S, That](pf: PartialFunction[T, S], cb: Combiner[S, That]): Combiner[S, That] = {
      //val cb = pbf(self.repr)
      collect2combiner_quick(pf, arr, cb, until, i)
      i = until
      cb
    }

    private def collect2combiner_quick[S, That](pf: PartialFunction[T, S], a: Array[Any], cb: Builder[S, That], ntil: Int, from: Int) {
      var j = from
      val runWith = pf.runWith(b => cb += b)
      while (j < ntil) {
        val curr = a(j).asInstanceOf[T]
        runWith(curr)
        j += 1
      }
    }

    override def flatmap2combiner[S, That](f: T => GenTraversableOnce[S], cb: Combiner[S, That]): Combiner[S, That] = {
      //val cb = pbf(self.repr)
      while (i < until) {
        val traversable = f(arr(i).asInstanceOf[T])
        if (traversable.isInstanceOf[Iterable[_]]) cb ++= traversable.asInstanceOf[Iterable[S]].iterator
        else cb ++= traversable.seq
        i += 1
      }
      cb
    }

    override def filter2combiner[U >: T, This](pred: T => Boolean, cb: Combiner[U, This]) = {
      filter2combiner_quick(pred, cb, arr, until, i)
      i = until
      cb
    }

    private def filter2combiner_quick[U >: T, This](pred: T => Boolean, cb: Builder[U, This], a: Array[Any], ntil: Int, from: Int) {
      var j = i
      while(j < ntil) {
        val curr = a(j).asInstanceOf[T]
        if (pred(curr)) cb += curr
        j += 1
      }
    }

    override def filterNot2combiner[U >: T, This](pred: T => Boolean, cb: Combiner[U, This]) = {
      filterNot2combiner_quick(pred, cb, arr, until, i)
      i = until
      cb
    }

    private def filterNot2combiner_quick[U >: T, This](pred: T => Boolean, cb: Builder[U, This], a: Array[Any], ntil: Int, from: Int) {
      var j = i
      while(j < ntil) {
        val curr = a(j).asInstanceOf[T]
        if (!pred(curr)) cb += curr
        j += 1
      }
    }

    override def copy2builder[U >: T, Coll, Bld <: Builder[U, Coll]](cb: Bld): Bld = {
      cb.sizeHint(remaining)
      cb.ifIs[ResizableParArrayCombiner[T]] {
      pac =>
        // with res. combiner:
        val targetarr: Array[Any] = pac.lastbuff.internalArray.asInstanceOf[Array[Any]]
        Array.copy(arr, i, targetarr, pac.lastbuff.size, until - i)
        pac.lastbuff.setInternalSize(remaining)
      } otherwise {
        cb.ifIs[UnrolledParArrayCombiner[T]] {
          pac =>
            // with unr. combiner:
            val targetarr: Array[Any] = pac.buff.lastPtr.array.asInstanceOf[Array[Any]]
          Array.copy(arr, i, targetarr, 0, until - i)
          pac.buff.size = pac.buff.size + until - i
          pac.buff.lastPtr.size = until - i
        } otherwise {
          copy2builder_quick(cb, arr, until, i)
          i = until
        }
      }
      cb
    }

    private def copy2builder_quick[U >: T, Coll](b: Builder[U, Coll], a: Array[Any], ntil: Int, from: Int) {
      var j = from
      while (j < ntil) {
        b += a(j).asInstanceOf[T]
        j += 1
      }
    }

    override def partition2combiners[U >: T, This](pred: T => Boolean, btrue: Combiner[U, This], bfalse: Combiner[U, This]) = {
      partition2combiners_quick(pred, btrue, bfalse, arr, until, i)
      i = until
      (btrue, bfalse)
    }

    private def partition2combiners_quick[U >: T, This](p: T => Boolean, btrue: Builder[U, This], bfalse: Builder[U, This], a: Array[Any], ntil: Int, from: Int) {
      var j = from
      while (j < ntil) {
        val curr = a(j).asInstanceOf[T]
        if (p(curr)) btrue += curr else bfalse += curr
        j += 1
      }
    }

    override def take2combiner[U >: T, This](n: Int, cb: Combiner[U, This]) = {
      cb.sizeHint(n)
      val ntil = i + n
      val a = arr
      while (i < ntil) {
        cb += a(i).asInstanceOf[T]
        i += 1
      }
      cb
    }

    override def drop2combiner[U >: T, This](n: Int, cb: Combiner[U, This]) = {
      drop(n)
      cb.sizeHint(remaining)
      while (i < until) {
        cb += arr(i).asInstanceOf[T]
        i += 1
      }
      cb
    }

    override def reverse2combiner[U >: T, This](cb: Combiner[U, This]): Combiner[U, This] = {
      cb.ifIs[ResizableParArrayCombiner[T]] {
      pac =>
        // with res. combiner:
        val sz = remaining
        pac.sizeHint(sz)
        val targetarr: Array[Any] = pac.lastbuff.internalArray.asInstanceOf[Array[Any]]
        reverse2combiner_quick(targetarr, arr, 0, i, until)
        pac.lastbuff.setInternalSize(sz)
      } otherwise {
        cb.ifIs[UnrolledParArrayCombiner[T]] {
          pac =>
            // with unr. combiner:
            val sz = remaining
          pac.sizeHint(sz)
          val targetarr: Array[Any] = pac.buff.lastPtr.array.asInstanceOf[Array[Any]]
          reverse2combiner_quick(targetarr, arr, 0, i, until)
          pac.buff.size = pac.buff.size + sz
          pac.buff.lastPtr.size = sz
        } otherwise super.reverse2combiner(cb)
      }
      cb
    }

    private def reverse2combiner_quick(targ: Array[Any], a: Array[Any], targfrom: Int, srcfrom: Int, srcuntil: Int) {
      var j = srcfrom
      var k = targfrom + srcuntil - srcfrom - 1
      while (j < srcuntil) {
        targ(k) = a(j)
        j += 1
        k -= 1
      }
    }

    override def scanToArray[U >: T, A >: U](z: U, op: (U, U) => U, destarr: Array[A], from: Int) {
      scanToArray_quick[U](array, destarr.asInstanceOf[Array[Any]], op, z, i, until, from)
      i = until
    }

    protected def scanToArray_quick[U](srcarr: Array[Any], destarr: Array[Any], op: (U, U) => U, z: U, srcfrom: Int, srcntil: Int, destfrom: Int) {
      var last = z
      var j = srcfrom
      var k = destfrom
      while (j < srcntil) {
        last = op(last, srcarr(j).asInstanceOf[U])
        destarr(k) = last
        j += 1
        k += 1
      }
    }

  }

  /* operations */

  private def buildsArray[S, That](c: Builder[S, That]) = c.isInstanceOf[ParArrayCombiner[_]]

  override def map[S, That](f: T => S)(implicit bf: CanBuildFrom[ParArray[T], S, That]) = if (buildsArray(bf(repr))) {
    // reserve an array
    val targarrseq = new ArraySeq[S](length)
    val targetarr = targarrseq.array.asInstanceOf[Array[Any]]

    // fill it in parallel
    tasksupport.executeAndWaitResult(new Map[S](f, targetarr, 0, length))

    // wrap it into a parallel array
    (new ParArray[S](targarrseq)).asInstanceOf[That]
  } else super.map(f)(bf)

  override def scan[U >: T, That](z: U)(op: (U, U) => U)(implicit cbf: CanBuildFrom[ParArray[T], U, That]): That =
    if (tasksupport.parallelismLevel > 1 && buildsArray(cbf(repr))) {
      // reserve an array
      val targarrseq = new ArraySeq[U](length + 1)
      val targetarr = targarrseq.array.asInstanceOf[Array[Any]]
      targetarr(0) = z

      // do a parallel prefix scan
      if (length > 0) tasksupport.executeAndWaitResult(new CreateScanTree[U](0, size, z, op, splitter) mapResult {
        tree => tasksupport.executeAndWaitResult(new ScanToArray(tree, z, op, targetarr))
      })

      // wrap the array into a parallel array
      (new ParArray[U](targarrseq)).asInstanceOf[That]
    } else super.scan(z)(op)(cbf)

  /* tasks */

  class ScanToArray[U >: T](tree: ScanTree[U], z: U, op: (U, U) => U, targetarr: Array[Any])
  extends Task[Unit, ScanToArray[U]] {
    var result = ()

    def leaf(prev: Option[Unit]) = iterate(tree)
    private def iterate(tree: ScanTree[U]): Unit = tree match {
      case ScanNode(left, right) =>
        iterate(left)
        iterate(right)
      case ScanLeaf(_, _, from, len, Some(prev), _) =>
        scanLeaf(array, targetarr, from, len, prev.acc)
      case ScanLeaf(_, _, from, len, None, _) =>
        scanLeaf(array, targetarr, from, len, z)
    }
    private def scanLeaf(srcarr: Array[Any], targetarr: Array[Any], from: Int, len: Int, startval: U) {
      var i = from
      val until = from + len
      var curr = startval
      val operation = op
      while (i < until) {
        curr = operation(curr, srcarr(i).asInstanceOf[U])
        i += 1
        targetarr(i) = curr
      }
    }
    def split = tree match {
      case ScanNode(left, right) => Seq(
        new ScanToArray(left, z, op, targetarr),
        new ScanToArray(right, z, op, targetarr)
      )
      case _ => sys.error("Can only split scan tree internal nodes.")
    }
    def shouldSplitFurther = tree match {
      case ScanNode(_, _) => true
      case _ => false
    }
  }

  class Map[S](f: T => S, targetarr: Array[Any], offset: Int, howmany: Int) extends Task[Unit, Map[S]] {
    var result = ()

    def leaf(prev: Option[Unit]) = {
      val tarr = targetarr
      val sarr = array
      var i = offset
      val until = offset + howmany
      while (i < until) {
        tarr(i) = f(sarr(i).asInstanceOf[T])
        i += 1
      }
    }
    def split = {
      val fp = howmany / 2
      List(new Map(f, targetarr, offset, fp), new Map(f, targetarr, offset + fp, howmany - fp))
    }
    def shouldSplitFurther = howmany > scala.collection.parallel.thresholdFromSize(length, tasksupport.parallelismLevel)
  }

  /* serialization */

  private def writeObject(out: java.io.ObjectOutputStream) {
    out.defaultWriteObject
  }

  private def readObject(in: java.io.ObjectInputStream) {
    in.defaultReadObject

    // get raw array from arrayseq
    array = arrayseq.array.asInstanceOf[Array[Any]]
  }

}


/** $factoryInfo
 *  @define Coll `mutable.ParArray`
 *  @define coll parallel array
 */
object ParArray extends ParFactory[ParArray] {
  implicit def canBuildFrom[T]: CanCombineFrom[Coll, T, ParArray[T]] = new GenericCanCombineFrom[T]
  def newBuilder[T]: Combiner[T, ParArray[T]] = newCombiner
  def newCombiner[T]: Combiner[T, ParArray[T]] = ParArrayCombiner[T]

  /** Creates a new parallel array by wrapping the specified array.
   */
  def handoff[T](arr: Array[T]): ParArray[T] = wrapOrRebuild(arr, arr.length)

  /** Creates a new parallel array by wrapping a part of the specified array.
   */
  def handoff[T](arr: Array[T], sz: Int): ParArray[T] = wrapOrRebuild(arr, sz)

  private def wrapOrRebuild[T](arr: AnyRef, sz: Int) = arr match {
    case arr: Array[AnyRef] => new ParArray[T](new ExposedArraySeq[T](arr, sz))
    case _ => new ParArray[T](new ExposedArraySeq[T](scala.runtime.ScalaRunTime.toObjectArray(arr), sz))
  }

  def createFromCopy[T <: AnyRef : ClassTag](arr: Array[T]): ParArray[T] = {
    val newarr = new Array[T](arr.length)
    Array.copy(arr, 0, newarr, 0, arr.length)
    handoff(newarr)
  }

  def fromTraversables[T](xss: GenTraversableOnce[T]*) = {
    val cb = ParArrayCombiner[T]()
    for (xs <- xss) {
      cb ++= xs.seq
    }
    cb.result
  }

}
