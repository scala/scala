package scala.collection.parallel


import scala.collection.Parallel
import scala.collection.SeqLike
import scala.collection.generic.DefaultSignalling
import scala.collection.generic.AtomicIndexFlag
import scala.collection.generic.CanBuildFrom
import scala.collection.generic.CanCombineFrom
import scala.collection.generic.VolatileAbort




// TODO update docs!!
/** A template trait for sequences of type `ParSeq[T]`, representing
 *  parallel sequences with element type `T`.
 *
 *  $parallelseqinfo
 *
 *  @tparam T        the type of the elements contained in this collection
 *  @tparam Repr     the type of the actual collection containing the elements
 *
 *  @define parallelseqinfo
 *  Parallel sequences inherit the `IndexedSeq` trait. This means they provide
 *  efficient indexing and length computations. Like their sequential counterparts
 *  they always have a defined order of elements. This means they will produce resulting
 *  parallel sequences in the same way sequential sequences do. However, the order
 *  in which they iterate over elements to produce results is not defined and is generally
 *  nondeterministic. If the higher-order functions given to them produce no sideeffects,
 *  then this won't be noticeable.
 *
 *  This trait defines a new, more general `split` operation and reimplements the `split`
 *  operation of `ParallelIterable` trait using the new `split` operation.
 *
 *  @author prokopec
 *  @since 2.8
 */
trait ParSeqLike[+T, +Repr <: Parallel, +Sequential <: Seq[T] with SeqLike[T, Sequential]]
extends scala.collection.SeqLike[T, Repr]
   with ParIterableLike[T, Repr, Sequential] {
self =>

  type SuperParIterator = ParIterableIterator[T]

  /** An iterator that can be split into arbitrary subsets of iterators.
   *  The self-type requirement ensures that the signal context passing behaviour gets mixed in
   *  the concrete iterator instance in some concrete collection.
   *
   *  '''Note:''' In concrete collection classes, collection implementers might want to override the iterator
   *  `reverse2builder` method to ensure higher efficiency.
   */
  trait ParIterator extends ParSeqIterator[T] with super.ParIterator {
  me: SignalContextPassingIterator[ParIterator] =>
    def split: Seq[ParIterator]
    def psplit(sizes: Int*): Seq[ParIterator]
  }

  /** A stackable modification that ensures signal contexts get passed along the iterators.
   *  A self-type requirement in `ParallelIterator` ensures that this trait gets mixed into
   *  concrete iterators.
   */
  trait SignalContextPassingIterator[+IterRepr <: ParIterator]
  extends ParIterator with super.SignalContextPassingIterator[IterRepr] {
    // Note: See explanation in `ParallelIterableLike.this.SignalContextPassingIterator`
    // to understand why we do the cast here, and have a type parameter.
    // Bottomline: avoiding boilerplate and fighting against inability to override stackable modifications.
    abstract override def psplit(sizes: Int*): Seq[IterRepr] = {
      val pits = super.psplit(sizes: _*)
      pits foreach { _.signalDelegate = signalDelegate }
      pits.asInstanceOf[Seq[IterRepr]]
    }
  }

  /** A convenient shorthand for the signal context passing stackable modification.
   */
  type SCPI <: SignalContextPassingIterator[ParIterator]

  /** A more refined version of the iterator found in the `ParallelIterable` trait,
   *  this iterator can be split into arbitrary subsets of iterators.
   *
   *  @return       an iterator that can be split into subsets of precise size
   */
  protected def parallelIterator: ParIterator

  override def iterator: PreciseSplitter[T] = parallelIterator

  override def size = length

  /** Used to iterate elements using indices */
  protected abstract class Elements(start: Int, val end: Int) extends ParIterator with BufferedIterator[T] {
    me: SignalContextPassingIterator[ParIterator] =>

    private var i = start

    def hasNext = i < end

    def next: T = if (i < end) {
      val x = self(i)
      i += 1
      x
    } else Iterator.empty.next

    def head = self(i)

    final def remaining = end - i

    def split = psplit(remaining / 2, remaining - remaining / 2)

    def psplit(sizes: Int*) = {
      val incr = sizes.scanLeft(0)(_ + _)
      for ((from, until) <- incr.init zip incr.tail) yield {
        new Elements(start + from, (start + until) min end) with SignalContextPassingIterator[ParIterator]
      }
    }

    override def toString = "Elements(" + start + ", " + end + ")"
  }

  /* ParallelSeq methods */

  /** Returns the length of the longest segment of elements starting at
   *  a given position satisfying some predicate.
   *
   *  $indexsignalling
   *
   *  The index flag is initially set to maximum integer value.
   *
   *  @param p     the predicate used to test the elements
   *  @param from  the starting offset for the search
   *  @return      the length of the longest segment of elements starting at `from` and
   *               satisfying the predicate
   */
  override def segmentLength(p: T => Boolean, from: Int): Int = if (from >= length) 0 else {
    val realfrom = if (from < 0) 0 else from
    val ctx = new DefaultSignalling with AtomicIndexFlag
    ctx.setIndexFlag(Int.MaxValue)
    executeAndWaitResult(new SegmentLength(p, 0, parallelIterator.psplit(realfrom, length - realfrom)(1) assign ctx))._1
  }

  override def prefixLength(p: T => Boolean) = segmentLength(p, 0)

  /** Finds the first element satisfying some predicate.
   *
   *  $indexsignalling
   *
   *  The index flag is initially set to maximum integer value.
   *
   *  @param p     the predicate used to test the elements
   *  @param from  the starting offset for the search
   *  @return      the index `>= from` of the first element of this $coll that satisfies the predicate `p`,
   *               or `-1`, if none exists
   */
  override def indexWhere(p: T => Boolean, from: Int): Int = if (from >= length) -1 else {
    val realfrom = if (from < 0) 0 else from
    val ctx = new DefaultSignalling with AtomicIndexFlag
    ctx.setIndexFlag(Int.MaxValue)
    executeAndWaitResult(new IndexWhere(p, realfrom, parallelIterator.psplit(realfrom, length - realfrom)(1) assign ctx))
  }

  override def indexWhere(p: T => Boolean): Int = indexWhere(p, 0)

  override def findIndexOf(p: T => Boolean): Int = indexWhere(p, 0)

  override def indexOf[U >: T](elem: U): Int = indexOf(elem, 0)

  override def indexOf[U >: T](elem: U, from: Int): Int = indexWhere(elem ==, from)

  /** Finds the last element satisfying some predicate.
   *
   *  $indexsignalling
   *
   *  The index flag is initially set to minimum integer value.
   *
   *  @param p     the predicate used to test the elements
   *  @param end   the maximum offset for the search
   *  @return      the index `<= end` of the first element of this $coll that satisfies the predicate `p`,
   *               or `-1`, if none exists
   */
  override def lastIndexWhere(p: T => Boolean, end: Int): Int = if (end < 0) -1 else {
    val until = if (end >= length) length else end + 1
    val ctx = new DefaultSignalling with AtomicIndexFlag
    ctx.setIndexFlag(Int.MinValue)
    executeAndWaitResult(new LastIndexWhere(p, 0, parallelIterator.psplit(until, length - until)(0) assign ctx))
  }

  override def reverse: Repr = {
    executeAndWaitResult(new Reverse(() => newCombiner, parallelIterator) mapResult { _.result })
  }

  override def reverseMap[S, That](f: T => S)(implicit bf: CanBuildFrom[Repr, S, That]): That = bf ifParallel { pbf =>
    executeAndWaitResult(new ReverseMap[S, That](f, pbf, parallelIterator) mapResult { _.result })
  } otherwise super.reverseMap(f)(bf)

  override def startsWith[S](that: Seq[S]): Boolean = startsWith(that, 0)

  /** Tests whether this $coll contains the given sequence at a given index.
   *
   *  $abortsignalling
   *
   *  @tparam U      the element type of `that` parallel sequence
   *  @param that    the parallel sequence this sequence is being searched for
   *  @param offset  the starting offset for the search
   *  @return        `true` if there is a sequence `that` starting at `offset` in this sequence, `false` otherwise
   */
  override def startsWith[S](that: Seq[S], offset: Int): Boolean = that ifParSeq { pthat =>
    if (offset < 0 || offset >= length) offset == length && pthat.length == 0
    else if (pthat.length == 0) true
    else if (pthat.length > length - offset) false
    else {
      val ctx = new DefaultSignalling with VolatileAbort
      executeAndWaitResult(new SameElements(parallelIterator.psplit(offset, pthat.length)(1) assign ctx, pthat.parallelIterator))
    }
  } otherwise super.startsWith(that, offset)

  override def sameElements[U >: T](that: Iterable[U]): Boolean = that ifParSeq { pthat =>
    val ctx = new DefaultSignalling with VolatileAbort
    length == pthat.length && executeAndWaitResult(new SameElements(parallelIterator assign ctx, pthat.parallelIterator))
  } otherwise super.sameElements(that)

  /** Tests whether this $coll ends with the given parallel sequence
   *
   *  $abortsignalling
   *
   *  @tparam S       the type of the elements of `that` sequence
   *  @param that     the sequence to test
   *  @return         `true` if this $coll has `that` as a suffix, `false` otherwise
   */
  override def endsWith[S](that: Seq[S]): Boolean = that ifParSeq { pthat =>
    if (that.length == 0) true
    else if (that.length > length) false
    else {
      val ctx = new DefaultSignalling with VolatileAbort
      val tlen = that.length
      executeAndWaitResult(new SameElements(parallelIterator.psplit(length - tlen, tlen)(1) assign ctx, pthat.parallelIterator))
    }
  } otherwise super.endsWith(that)

  override def patch[U >: T, That](from: Int, patch: Seq[U], replaced: Int)
  (implicit bf: CanBuildFrom[Repr, U, That]): That = if (patch.isParSeq && bf.isParallel) {
    val that = patch.asParSeq
    val pbf = bf.asParallel
    val realreplaced = replaced min (length - from)
    val pits = parallelIterator.psplit(from, replaced, length - from - realreplaced)
    val copystart = new Copy[U, That](() => pbf(repr), pits(0))
    val copymiddle = wrap {
      val tsk = new that.Copy[U, That](() => pbf(repr), that.parallelIterator)
      tsk.compute
      tsk.result
    }
    val copyend = new Copy[U, That](() => pbf(repr), pits(2))
    executeAndWaitResult(((copystart parallel copymiddle) { _ combine _ } parallel copyend) { _ combine _ } mapResult { _.result })
  } else patch_sequential(from, patch, replaced)

  private def patch_sequential[U >: T, That](from: Int, patch: Seq[U], r: Int)(implicit bf: CanBuildFrom[Repr, U, That]): That = {
    val b = bf(repr)
    val repl = r min (length - from)
    val pits = parallelIterator.psplit(from, repl, length - from - repl)
    b ++= pits(0)
    b ++= patch.iterator
    b ++= pits(2)
    b.result
  }

  override def updated[U >: T, That](index: Int, elem: U)(implicit bf: CanBuildFrom[Repr, U, That]): That = bf ifParallel { pbf =>
    executeAndWaitResult(new Updated(index, elem, pbf, parallelIterator) mapResult { _.result })
  } otherwise super.updated(index, elem)

  override def +:[U >: T, That](elem: U)(implicit bf: CanBuildFrom[Repr, U, That]): That = {
    patch(0, mutable.ParArray(elem), 0)
  }

  override def :+[U >: T, That](elem: U)(implicit bf: CanBuildFrom[Repr, U, That]): That = {
    patch(length, mutable.ParArray(elem), 0)
  }

  override def padTo[U >: T, That](len: Int, elem: U)(implicit bf: CanBuildFrom[Repr, U, That]): That = if (length < len) {
    patch(length, new immutable.Repetition(elem, len - length), 0)
  } else patch(length, Nil, 0);

  override def zip[U >: T, S, That](that: Iterable[S])(implicit bf: CanBuildFrom[Repr, (U, S), That]): That = if (bf.isParallel && that.isParSeq) {
    val pbf = bf.asParallel
    val thatseq = that.asParSeq
    executeAndWaitResult(new Zip(length min thatseq.length, pbf, parallelIterator, thatseq.parallelIterator) mapResult { _.result });
  } else super.zip(that)(bf)

  /** Tests whether every element of this $coll relates to the
   *  corresponding element of another parallel sequence by satisfying a test predicate.
   *
   *  $abortsignalling
   *
   *  @param   that    the other parallel sequence
   *  @param   p       the test predicate, which relates elements from both sequences
   *  @tparam  S       the type of the elements of `that`
   *  @return          `true` if both parallel sequences have the same length and
   *                   `p(x, y)` is `true` for all corresponding elements `x` of this $coll
   *                   and `y` of `that`, otherwise `false`
   */
  override def corresponds[S](that: Seq[S])(p: (T, S) => Boolean): Boolean = that ifParSeq { pthat =>
    val ctx = new DefaultSignalling with VolatileAbort
    length == pthat.length && executeAndWaitResult(new Corresponds(p, parallelIterator assign ctx, pthat.parallelIterator))
  } otherwise super.corresponds(that)(p)

  override def toString = seq.mkString(stringPrefix + "(", ", ", ")")

  override def view = new ParSeqView[T, Repr, Sequential] {
    protected lazy val underlying = self.repr
    def length = self.length
    def apply(idx: Int) = self(idx)
    def seq = self.seq.view
    def parallelIterator = new Elements(0, length) with SCPI {}
  }

  override def view(from: Int, until: Int) = view.slice(from, until)

  /* tasks */

  protected[this] def down(p: ParIterableIterator[_]) = p.asInstanceOf[ParSeqIterator[T]]

  protected trait Accessor[R, Tp] extends super.Accessor[R, Tp] {
    protected[this] val pit: ParSeqIterator[T]
  }

  protected trait Transformer[R, Tp] extends Accessor[R, Tp] with super.Transformer[R, Tp]

  protected[this] class SegmentLength(pred: T => Boolean, from: Int, protected[this] val pit: ParSeqIterator[T])
  extends Accessor[(Int, Boolean), SegmentLength] {
    var result: (Int, Boolean) = null
    def leaf(prev: Option[(Int, Boolean)]) = if (from < pit.indexFlag) {
      val itsize = pit.remaining
      val seglen = pit.prefixLength(pred)
      result = (seglen, itsize == seglen)
      if (!result._2) pit.setIndexFlagIfLesser(from)
    } else result = (0, false)
    protected[this] def newSubtask(p: SuperParIterator) = throw new UnsupportedOperationException
    override def split = {
      val pits = pit.split
      for ((p, untilp) <- pits zip pits.scanLeft(0)(_ + _.remaining)) yield new SegmentLength(pred, from + untilp, p)
    }
    override def merge(that: SegmentLength) = if (result._2) result = (result._1 + that.result._1, that.result._2)
  }

  protected[this] class IndexWhere(pred: T => Boolean, from: Int, protected[this] val pit: ParSeqIterator[T])
  extends Accessor[Int, IndexWhere] {
    var result: Int = -1
    def leaf(prev: Option[Int]) = if (from < pit.indexFlag) {
      val r = pit.indexWhere(pred)
      if (r != -1) {
        result = from + r
        pit.setIndexFlagIfLesser(from)
      }
    }
    protected[this] def newSubtask(p: SuperParIterator) = throw new UnsupportedOperationException
    override def split = {
      val pits = pit.split
      for ((p, untilp) <- pits zip pits.scanLeft(from)(_ + _.remaining)) yield new IndexWhere(pred, untilp, p)
    }
    override def merge(that: IndexWhere) = result = if (result == -1) that.result else {
      if (that.result != -1) result min that.result else result
    }
  }

  protected[this] class LastIndexWhere(pred: T => Boolean, pos: Int, protected[this] val pit: ParSeqIterator[T])
  extends Accessor[Int, LastIndexWhere] {
    var result: Int = -1
    def leaf(prev: Option[Int]) = if (pos > pit.indexFlag) {
      val r = pit.lastIndexWhere(pred)
      if (r != -1) {
        result = pos + r
        pit.setIndexFlagIfGreater(pos)
      }
    }
    protected[this] def newSubtask(p: SuperParIterator) = throw new UnsupportedOperationException
    override def split = {
      val pits = pit.split
      for ((p, untilp) <- pits zip pits.scanLeft(pos)(_ + _.remaining)) yield new LastIndexWhere(pred, untilp, p)
    }
    override def merge(that: LastIndexWhere) = result = if (result == -1) that.result else {
      if (that.result != -1) result max that.result else result
    }
  }

  protected[this] class Reverse[U >: T, This >: Repr](cbf: () => Combiner[U, This], protected[this] val pit: ParSeqIterator[T])
  extends Transformer[Combiner[U, This], Reverse[U, This]] {
    var result: Combiner[U, This] = null
    def leaf(prev: Option[Combiner[U, This]]) = result = pit.reverse2combiner(reuse(prev, cbf()))
    protected[this] def newSubtask(p: SuperParIterator) = new Reverse(cbf, down(p))
    override def merge(that: Reverse[U, This]) = result = that.result combine result
  }

  protected[this] class ReverseMap[S, That](f: T => S, pbf: CanCombineFrom[Repr, S, That], protected[this] val pit: ParSeqIterator[T])
  extends Transformer[Combiner[S, That], ReverseMap[S, That]] {
    var result: Combiner[S, That] = null
    def leaf(prev: Option[Combiner[S, That]]) = result = pit.reverseMap2combiner(f, pbf(self.repr))
    protected[this] def newSubtask(p: SuperParIterator) = new ReverseMap(f, pbf, down(p))
    override def merge(that: ReverseMap[S, That]) = result = that.result combine result
  }

  protected[this] class SameElements[U >: T](val pit: ParIterator, val otherpit: PreciseSplitter[U])
  extends Accessor[Boolean, SameElements[U]] {
    var result: Boolean = true
    def leaf(prev: Option[Boolean]) = if (!pit.isAborted) {
      result = pit.sameElements(otherpit)
      if (!result) pit.abort
    }
    protected[this] def newSubtask(p: SuperParIterator) = unsupported
    override def split = {
      val fp = pit.remaining / 2
      val sp = pit.remaining - fp
      for ((p, op) <- pit.psplit(fp, sp) zip otherpit.psplit(fp, sp)) yield new SameElements(p, op)
    }
    override def merge(that: SameElements[U]) = result = result && that.result
  }

  protected[this] class Updated[U >: T, That](pos: Int, elem: U, pbf: CanCombineFrom[Repr, U, That], protected[this] val pit: ParSeqIterator[T])
  extends Transformer[Combiner[U, That], Updated[U, That]] {
    var result: Combiner[U, That] = null
    def leaf(prev: Option[Combiner[U, That]]) = result = pit.updated2combiner(pos, elem, pbf(self.repr))
    protected[this] def newSubtask(p: SuperParIterator) = unsupported
    override def split = {
      val pits = pit.split
      for ((p, untilp) <- pits zip pits.scanLeft(0)(_ + _.remaining)) yield new Updated(pos - untilp, elem, pbf, p)
    }
    override def merge(that: Updated[U, That]) = result = result combine that.result
  }

  protected[this] class Zip[U >: T, S, That](len: Int, pbf: CanCombineFrom[Repr, (U, S), That], val pit: ParIterator, val otherpit: PreciseSplitter[S])
  extends Transformer[Combiner[(U, S), That], Zip[U, S, That]] {
    var result: Result = null
    def leaf(prev: Option[Result]) = result = pit.zip2combiner[U, S, That](otherpit, pbf(self.repr))
    protected[this] def newSubtask(p: SuperParIterator) = unsupported
    override def split = {
      val fp = len / 2
      val sp = len - len / 2
      val pits = pit.psplit(fp, sp)
      val opits = otherpit.psplit(fp, sp)
      Seq(
        new Zip(fp, pbf, pits(0), opits(0)),
        new Zip(sp, pbf, pits(1), opits(1))
      )
    }
    override def merge(that: Zip[U, S, That]) = result = result combine that.result
  }

  protected[this] class Corresponds[S](corr: (T, S) => Boolean, val pit: ParIterator, val otherpit: PreciseSplitter[S])
  extends Accessor[Boolean, Corresponds[S]] {
    var result: Boolean = true
    def leaf(prev: Option[Boolean]) = if (!pit.isAborted) {
      result = pit.corresponds(corr)(otherpit)
      if (!result) pit.abort
    }
    protected[this] def newSubtask(p: SuperParIterator) = unsupported
    override def split = {
      val fp = pit.remaining / 2
      val sp = pit.remaining - fp
      for ((p, op) <- pit.psplit(fp, sp) zip otherpit.psplit(fp, sp)) yield new Corresponds(corr, p, op)
    }
    override def merge(that: Corresponds[S]) = result = result && that.result
  }

}




































