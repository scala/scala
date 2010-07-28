package scala.collection.parallel




import scala.collection.mutable.Builder
import scala.collection.mutable.ListBuffer
import scala.collection.IterableLike
import scala.collection.Parallel
import scala.collection.Parallelizable
import scala.collection.Sequentializable
import scala.collection.generic._




// TODO update docs!!
/** A template trait for parallel collections of type `ParIterable[T]`.
 *
 *  $paralleliterableinfo
 *
 *  $sideeffects
 *
 *  @tparam T    the element type of the collection
 *  @tparam Repr the type of the actual collection containing the elements
 *
 *  @define paralleliterableinfo
 *  This is a base trait for Scala parallel collections. It defines behaviour
 *  common to all parallel collections. The actual parallel operation implementation
 *  is found in the `ParallelIterableFJImpl` trait extending this trait. Concrete
 *  parallel collections should inherit both this and that trait.
 *
 *  Parallel operations are implemented with divide and conquer style algorithms that
 *  parallelize well. The basic idea is to split the collection into smaller parts until
 *  they are small enough to be operated on sequentially.
 *
 *  All of the parallel operations are implemented in terms of several methods. The first is:
 *  {{{
 *     def split: Seq[Repr]
 *  }}}
 *  which splits the collection into a sequence of disjunct views. This is typically a
 *  very fast operation which simply creates wrappers around the receiver collection.
 *  These views can then be split recursively into smaller views and so on. Each of
 *  the views is still a parallel collection.
 *
 *  The next method is:
 *  {{{
 *     def combine[OtherRepr >: Repr](other: OtherRepr): OtherRepr
 *  }}}
 *  which combines this collection with the argument collection and returns a collection
 *  containing both the elements of this collection and the argument collection. This behaviour
 *  may be implemented by producing a view that iterates over both collections, by aggressively
 *  copying all the elements into the new collection or by lazily creating a wrapper over both
 *  collections that gets evaluated once it's needed. It is recommended to avoid copying all of
 *  the elements for performance reasons, although that cost might be negligible depending on
 *  the use case.
 *
 *  Methods:
 *  {{{
 *     def seq: Repr
 *  }}}
 *  and
 *  {{{
 *     def par: Repr
 *  }}}
 *  produce a view of the collection that has sequential or parallel operations, respectively.
 *
 *  The method:
 *  {{{
 *     def threshold(sz: Int, p: Int): Int
 *  }}}
 *  provides an estimate on the minimum number of elements the collection has before
 *  the splitting stops and depends on the number of elements in the collection. A rule of the
 *  thumb is the number of elements divided by 8 times the parallelism level. This method may
 *  be overridden in concrete implementations if necessary.
 *
 *  Finally, method `newCombiner` produces a new parallel builder.
 *
 *  Since this trait extends the `Iterable` trait, methods like `size` and `iterator` must also
 *  be implemented.
 *
 *  Each parallel collection is bound to a specific fork/join pool, on which dormant worker
 *  threads are kept. One can change a fork/join pool of a collection any time except during
 *  some method being invoked. The fork/join pool contains other information such as the parallelism
 *  level, that is, the number of processors used. When a collection is created, it is assigned the
 *  default fork/join pool found in the `scala.collection.parallel` package object.
 *
 *  Parallel collections may or may not be strict, and they are not ordered in terms of the `foreach`
 *  operation (see `Traversable`). In terms of the iterator of the collection, some collections
 *  are ordered (for instance, parallel sequences).
 *
 *  @author prokopec
 *  @since 2.8
 *
 *  @define sideeffects
 *  The higher-order functions passed to certain operations may contain side-effects. Since implementations
 *  of operations may not be sequential, this means that side-effects may not be predictable and may
 *  produce data-races, deadlocks or invalidation of state if care is not taken. It is up to the programmer
 *  to either avoid using side-effects or to use some form of synchronization when accessing mutable data.
 *
 *  @define undefinedorder
 *  The order in which the operations on elements are performed is unspecified and may be nondeterministic.
 *
 *  @define pbfinfo
 *  An implicit value of class `CanCombineFrom` which determines the
 *  result class `That` from the current representation type `Repr` and
 *  and the new element type `B`. This builder factory can provide a parallel
 *  builder for the resulting collection.
 *
 *  @define abortsignalling
 *  This method will provide sequential views it produces with `abort` signalling capabilities. This means
 *  that sequential views may send and read `abort` signals.
 *
 *  @define indexsignalling
 *  This method will provide sequential views it produces with `indexFlag` signalling capabilities. This means
 *  that sequential views may set and read `indexFlag` state.
 */
trait ParIterableLike[+T, +Repr <: Parallel, +SequentialView <: Iterable[T]]
extends IterableLike[T, Repr]
   with Parallelizable[Repr]
   with Sequentializable[T, SequentialView]
   with Parallel
   with HasNewCombiner[T, Repr]
   with TaskSupport {
  self =>

  /** Parallel iterators are split iterators that have additional accessor and
   *  transformer methods defined in terms of methods `next` and `hasNext`.
   *  When creating a new parallel collection, one might want to override these
   *  new methods to make them more efficient.
   *
   *  Parallel iterators are augmented with signalling capabilities. This means
   *  that a signalling object can be assigned to them as needed.
   *
   *  The self-type ensures that signal context passing behaviour gets mixed in
   *  a concrete object instance.
   */
  trait ParIterator extends ParIterableIterator[T, Repr] {
    me: SignalContextPassingIterator[ParIterator] =>
    var signalDelegate: Signalling = IdleSignalling
    def repr = self.repr
    def split: Seq[ParIterator]
  }

  /** A stackable modification that ensures signal contexts get passed along the iterators.
   *  A self-type requirement in `ParIterator` ensures that this trait gets mixed into
   *  concrete iterators.
   */
  trait SignalContextPassingIterator[+IterRepr <: ParIterator] extends ParIterator {
    // Note: This functionality must be factored out to this inner trait to avoid boilerplate.
    // Also, one could omit the cast below. However, this leads to return type inconsistencies,
    // due to inability to override the return type of _abstract overrides_.
    // Be aware that this stackable modification has to be subclassed, so it shouldn't be rigid
    // on the type of iterators it splits.
    // The alternative is some boilerplate - better to tradeoff some type safety to avoid it here.
    abstract override def split: Seq[IterRepr] = {
      val pits = super.split
      pits foreach { _.signalDelegate = signalDelegate }
      pits.asInstanceOf[Seq[IterRepr]]
    }
  }

  /** Convenience for signal context passing iterator.
   */
  type SCPI <: SignalContextPassingIterator[ParIterator]

  /** Creates a new parallel iterator used to traverse the elements of this parallel collection.
   *  This iterator is more specific than the iterator of the returned by `iterator`, and augmented
   *  with additional accessor and transformer methods.
   *
   *  @return          a parallel iterator
   */
  protected def parallelIterator: ParIterator

  /** Creates a new split iterator used to traverse the elements of this collection.
   *
   *  By default, this method is implemented in terms of the protected `parallelIterator` method.
   *
   *  @return         a split iterator
   */
  def iterator: Splitter[T] = parallelIterator

  def par = repr

  /** Some minimal number of elements after which this collection should be handled
   *  sequentially by different processors.
   *
   *  This method depends on the size of the collection and the parallelism level, which
   *  are both specified as arguments.
   *
   *  @param sz   the size based on which to compute the threshold
   *  @param p    the parallelism level based on which to compute the threshold
   *  @return     the maximum number of elements for performing operations sequentially
   */
  def threshold(sz: Int, p: Int): Int = thresholdFromSize(sz, p)

  /** The `newBuilder` operation returns a parallel builder assigned to this collection's fork/join pool.
   *  This method forwards the call to `newCombiner`.
   */
  protected[this] override def newBuilder: collection.mutable.Builder[T, Repr] = newCombiner

  /** Optionally reuses existing combiner for better performance. By default it doesn't - subclasses may override this behaviour.
   *  The provided combiner `oldc` that can potentially be reused will be either some combiner from the previous computational task, or `None` if there
   *  was no previous phase (in which case this method must return `newc`).
   *
   *  @param oldc   The combiner that is the result of the previous task, or `None` if there was no previous task.
   *  @param newc   The new, empty combiner that can be used.
   *  @return       Either `newc` or `oldc`.
   */
  protected def reuse[S, That](oldc: Option[Combiner[S, That]], newc: Combiner[S, That]): Combiner[S, That] = newc

  /* convenience task operations wrapper */
  protected implicit def task2ops[R, Tp](tsk: Task[R, Tp]) = new {
    def mapResult[R1](mapping: R => R1): ResultMapping[R, Tp, R1] = new ResultMapping[R, Tp, R1](tsk) {
      def map(r: R): R1 = mapping(r)
    }

    def compose[R3, R2, Tp2](t2: Task[R2, Tp2])(resCombiner: (R, R2) => R3) = new SeqComposite[R, R2, R3, Task[R, Tp], Task[R2, Tp2]] {
      val ft = tsk
      val st = t2
      def combineResults(fr: R, sr: R2): R3 = resCombiner(fr, sr)
    }

    def parallel[R3, R2, Tp2](t2: Task[R2, Tp2])(resCombiner: (R, R2) => R3) = new ParComposite[R, R2, R3, Task[R, Tp], Task[R2, Tp2]] {
      val ft = tsk
      val st = t2
      def combineResults(fr: R, sr: R2): R3 = resCombiner(fr, sr)
    }
  }

  protected def wrap[R](body: => R) = new NonDivisible[R] {
    def leaf(prevr: Option[R]) = result = body
    var result: R = null.asInstanceOf[R]
  }

  /* convenience iterator operations wrapper */
  protected implicit def iterator2ops[PI <: ParIterator](it: PI) = new {
    def assign(cntx: Signalling): PI = {
      it.signalDelegate = cntx
      it
    }
  }

  protected implicit def builder2ops[Elem, To](cb: Builder[Elem, To]) = new {
    def ifIs[Cmb](isbody: Cmb => Unit) = new {
      def otherwise(notbody: => Unit)(implicit m: ClassManifest[Cmb]) {
        if (cb.getClass == m.erasure) isbody(cb.asInstanceOf[Cmb]) else notbody
      }
    }
  }

  override def toString = seq.mkString(stringPrefix + "(", ", ", ")")

  /** Reduces the elements of this sequence using the specified associative binary operator.
   *
   *  $undefinedorder
   *
   *  Note this method has a different signature than the `reduceLeft`
   *  and `reduceRight` methods of the trait `Traversable`.
   *  The result of reducing may only be a supertype of this parallel collection's
   *  type parameter `T`.
   *
   *  @tparam U      A type parameter for the binary operator, a supertype of `T`.
   *  @param op       A binary operator that must be associative.
   *  @return         The result of applying reduce operator `op` between all the elements if the collection is nonempty.
   *  @throws UnsupportedOperationException
   *  if this $coll is empty.
   */
  def reduce[U >: T](op: (U, U) => U): U = {
    executeAndWaitResult(new Reduce(op, parallelIterator))
  }

  /** Optionally reduces the elements of this sequence using the specified associative binary operator.
   *
   *  $undefinedorder
   *
   *  Note this method has a different signature than the `reduceLeftOption`
   *  and `reduceRightOption` methods of the trait `Traversable`.
   *  The result of reducing may only be a supertype of this parallel collection's
   *  type parameter `T`.
   *
   *  @tparam U      A type parameter for the binary operator, a supertype of `T`.
   *  @param op      A binary operator that must be associative.
   *  @return        An option value containing result of applying reduce operator `op` between all
   *                 the elements if the collection is nonempty, and `None` otherwise.
   */
  def reduceOption[U >: T](op: (U, U) => U): Option[U] = if (isEmpty) None else Some(reduce(op))

  /** Folds the elements of this sequence using the specified associative binary operator.
   *  The order in which the elements are reduced is unspecified and may be nondeterministic.
   *
   *  Note this method has a different signature than the `foldLeft`
   *  and `foldRight` methods of the trait `Traversable`.
   *  The result of folding may only be a supertype of this parallel collection's
   *  type parameter `T`.
   *
   *  @tparam U      a type parameter for the binary operator, a supertype of `T`.
   *  @param z       a neutral element for the fold operation, it may be added to the result
   *                 an arbitrary number of times, not changing the result (e.g. `Nil` for list concatenation,
   *                 0 for addition, or 1 for multiplication)
   *  @param op      a binary operator that must be associative
   *  @return        the result of applying fold operator `op` between all the elements and `z`
   */
  def fold[U >: T](z: U)(op: (U, U) => U): U = {
    executeAndWaitResult(new Fold(z, op, parallelIterator))
  }

  /** Aggregates the results of applying an operator to subsequent elements.
   *
   *  This is a more general form of `fold` and `reduce`. It has similar semantics, but does
   *  not require the result to be a supertype of the element type. It traverses the elements in
   *  different partitions sequentially, using `seqop` to update the result, and then
   *  applies `combop` to results from different partitions. The implementation of this
   *  operation may operate on an arbitrary number of collection partitions, so `combop`
   *  may be invoked arbitrary number of times.
   *
   *  For example, one might want to process some elements and then produce a `Set`. In this
   *  case, `seqop` would process an element and append it to the list, while `combop`
   *  would concatenate two lists from different partitions together. The initial value
   *  `z` would be an empty set.
   *
   *  {{{
   *    pc.aggregate(Set[Int]())(_ += process(_), _ ++ _)
   *  }}}
   *
   *  Another example is calculating geometric mean from a collection of doubles
   *  (one would typically require big doubles for this).
   *
   *  @tparam S        the type of accumulated results
   *  @param z         the initial value for the accumulated result of the partition - this
   *                   will typically be the neutral element for the `seqop` operator (e.g.
   *                   `Nil` for list concatenation or `0` for summation)
   *  @param seqop     an operator used to accumulate results within a partition
   *  @param combop    an associative operator used to combine results from different partitions
   */
  def aggregate[S](z: S)(seqop: (S, T) => S, combop: (S, S) => S): S = {
    executeAndWaitResult(new Aggregate(z, seqop, combop, parallelIterator))
  }

  /** Applies a function `f` to all the elements of the receiver.
   *
   *  $undefinedorder
   *
   *  @tparam U    the result type of the function applied to each element, which is always discarded
   *  @param f     function that's applied to each element
   */
  override def foreach[U](f: T => U): Unit = {
    executeAndWait(new Foreach(f, parallelIterator))
  }

  override def count(p: T => Boolean): Int = {
    executeAndWaitResult(new Count(p, parallelIterator))
  }

  override def sum[U >: T](implicit num: Numeric[U]): U = {
    executeAndWaitResult(new Sum[U](num, parallelIterator))
  }

  override def product[U >: T](implicit num: Numeric[U]): U = {
    executeAndWaitResult(new Product[U](num, parallelIterator))
  }

  override def min[U >: T](implicit ord: Ordering[U]): T = {
    executeAndWaitResult(new Min(ord, parallelIterator)).asInstanceOf[T]
  }

  override def max[U >: T](implicit ord: Ordering[U]): T = {
    executeAndWaitResult(new Max(ord, parallelIterator)).asInstanceOf[T]
  }

  override def map[S, That](f: T => S)(implicit bf: CanBuildFrom[Repr, S, That]): That = bf ifParallel { pbf =>
    executeAndWaitResult(new Map[S, That](f, pbf, parallelIterator) mapResult { _.result })
  } otherwise super.map(f)(bf)

  override def collect[S, That](pf: PartialFunction[T, S])(implicit bf: CanBuildFrom[Repr, S, That]): That = bf ifParallel { pbf =>
    executeAndWaitResult(new Collect[S, That](pf, pbf, parallelIterator) mapResult { _.result })
  } otherwise super.collect(pf)(bf)

  override def flatMap[S, That](f: T => Traversable[S])(implicit bf: CanBuildFrom[Repr, S, That]): That = bf ifParallel { pbf =>
    executeAndWaitResult(new FlatMap[S, That](f, pbf, parallelIterator) mapResult { _.result })
  } otherwise super.flatMap(f)(bf)

  /** Tests whether a predicate holds for all elements of this $coll.
   *
   *  $abortsignalling
   *
   *  @param p    a predicate used to test elements
   *  @return     true if `p` holds for all elements, false otherwise
   */
  override def forall(pred: T => Boolean): Boolean = {
    executeAndWaitResult(new Forall(pred, parallelIterator assign new DefaultSignalling with VolatileAbort))
  }

  /** Tests whether a predicate holds for some element of this $coll.
   *
   *  $abortsignalling
   *
   *  @param p    a predicate used to test elements
   *  @return     true if `p` holds for some element, false otherwise
   */
  override def exists(pred: T => Boolean): Boolean = {
    executeAndWaitResult(new Exists(pred, parallelIterator assign new DefaultSignalling with VolatileAbort))
  }

  /** Finds some element in the collection for which the predicate holds, if such
   *  an element exists. The element may not necessarily be the first such element
   *  in the iteration order.
   *
   *  If there are multiple elements obeying the predicate, the choice is nondeterministic.
   *
   *  $abortsignalling
   *
   *  @param p     predicate used to test the elements
   *  @return      an option value with the element if such an element exists, or `None` otherwise
   */
  override def find(pred: T => Boolean): Option[T] = {
    executeAndWaitResult(new Find(pred, parallelIterator assign new DefaultSignalling with VolatileAbort))
  }

  protected[this] def cbfactory = () => newCombiner

  override def filter(pred: T => Boolean): Repr = {
    executeAndWaitResult(new Filter(pred, cbfactory, parallelIterator) mapResult { _.result })
  }

  override def filterNot(pred: T => Boolean): Repr = {
    executeAndWaitResult(new FilterNot(pred, cbfactory, parallelIterator) mapResult { _.result })
  }

  override def ++[U >: T, That](that: TraversableOnce[U])(implicit bf: CanBuildFrom[Repr, U, That]): That = {
    if (that.isParallel && bf.isParallel) {
      // println("case both are parallel")
      val other = that.asParIterable
      val pbf = bf.asParallel
      val copythis = new Copy(() => pbf(repr), parallelIterator)
      val copythat = wrap {
        val othtask = new other.Copy(() => pbf(self.repr), other.parallelIterator)
        othtask.compute
        othtask.result
      }
      val task = (copythis parallel copythat) { _ combine _ } mapResult { _.result }
      executeAndWaitResult(task)
    } else if (bf.isParallel) {
      // println("case parallel builder, `that` not parallel")
      val pbf = bf.asParallel
      val copythis = new Copy(() => pbf(repr), parallelIterator)
      val copythat = wrap {
        val cb = pbf(repr)
        for (elem <- that) cb += elem
        cb
      }
      executeAndWaitResult((copythis parallel copythat) { _ combine _ } mapResult { _.result })
    } else {
      // println("case not a parallel builder")
      val b = bf(repr)
      this.parallelIterator.copy2builder[U, That, Builder[U, That]](b)
      if (that.isInstanceOf[Parallel]) for (elem <- that.asInstanceOf[Iterable[U]].iterator) b += elem
      else for (elem <- that) b += elem
      b.result
    }
  }

  override def partition(pred: T => Boolean): (Repr, Repr) = {
    executeAndWaitResult(new Partition(pred, cbfactory, parallelIterator) mapResult { p => (p._1.result, p._2.result) })
  }

  override def take(n: Int): Repr = {
    val actualn = if (size > n) n else size
    if (actualn < MIN_FOR_COPY) take_sequential(actualn)
    else executeAndWaitResult(new Take(actualn, cbfactory, parallelIterator) mapResult { _.result })
  }

  private def take_sequential(n: Int) = {
    val cb = newCombiner
    cb.sizeHint(n)
    val it = parallelIterator
    var left = n
    while (left > 0) {
      cb += it.next
      left -= 1
    }
    cb.result
  }

  override def drop(n: Int): Repr = {
    val actualn = if (size > n) n else size
    if ((size - actualn) < MIN_FOR_COPY) drop_sequential(actualn)
    else executeAndWaitResult(new Drop(actualn, cbfactory, parallelIterator) mapResult { _.result })
  }

  private def drop_sequential(n: Int) = {
    val it = parallelIterator drop n
    val cb = newCombiner
    cb.sizeHint(size - n)
    while (it.hasNext) cb += it.next
    cb.result
  }

  override def slice(unc_from: Int, unc_until: Int): Repr = {
    val from = unc_from min size max 0
    val until = unc_until min size max from
    if ((until - from) <= MIN_FOR_COPY) slice_sequential(from, until)
    else executeAndWaitResult(new Slice(from, until, cbfactory, parallelIterator) mapResult { _.result })
  }

  private def slice_sequential(from: Int, until: Int): Repr = {
    val cb = newCombiner
    var left = until - from
    val it = parallelIterator drop from
    while (left > 0) {
      cb += it.next
      left -= 1
    }
    cb.result
  }

  override def splitAt(n: Int): (Repr, Repr) = {
    executeAndWaitResult(new SplitAt(n, cbfactory, parallelIterator) mapResult { p => (p._1.result, p._2.result) })
  }

  /** Computes a prefix scan of the elements of the collection.
   *
   *  Note: The neutral element `z` may be applied more than once.
   *
   *  @tparam U         element type of the resulting collection
   *  @tparam That      type of the resulting collection
   *  @param z          neutral element for the operator `op`
   *  @param op         the associative operator for the scan
   *  @param cbf        combiner factory which provides a combiner
   *  @return           a collection containing the prefix scan of the elements in the original collection
   *
   *  @usecase def scan(z: T)(op: (T, T) => T): $Coll[T]
   *
   *  @return           a new $coll containing the prefix scan of the elements in this $coll
   */
  def scan[U >: T, That](z: U)(op: (U, U) => U)(implicit cbf: CanCombineFrom[Repr, U, That]): That = {
    val array = new Array[Any](size + 1)
    array(0) = z
    executeAndWaitResult(new ScanToArray[U, Any](z, op, 1, size, array, parallelIterator) mapResult { u =>
      executeAndWaitResult(new FromArray(array, 0, size + 1, cbf) mapResult { _.result })
    })
  }

  /** Takes the longest prefix of elements that satisfy the predicate.
   *
   *  $indexsignalling
   *  The index flag is initially set to maximum integer value.
   *
   *  @param pred   the predicate used to test the elements
   *  @return       the longest prefix of this $coll of elements that satisy the predicate `pred`
   */
  override def takeWhile(pred: T => Boolean): Repr = {
    val cntx = new DefaultSignalling with AtomicIndexFlag
    cntx.setIndexFlag(Int.MaxValue)
    executeAndWaitResult(new TakeWhile(0, pred, cbfactory, parallelIterator assign cntx) mapResult { _._1.result })
  }

  /** Splits this $coll into a prefix/suffix pair according to a predicate.
   *
   *  $indexsignalling
   *  The index flag is initially set to maximum integer value.
   *
   *  @param pred   the predicate used to test the elements
   *  @return       a pair consisting of the longest prefix of the collection for which all
   *                the elements satisfy `pred`, and the rest of the collection
   */
  override def span(pred: T => Boolean): (Repr, Repr) = {
    val cntx = new DefaultSignalling with AtomicIndexFlag
    cntx.setIndexFlag(Int.MaxValue)
    executeAndWaitResult(new Span(0, pred, cbfactory, parallelIterator assign cntx) mapResult {
      p => (p._1.result, p._2.result)
    })
  }

  /** Drops all elements in the longest prefix of elements that satisfy the predicate,
   *  and returns a collection composed of the remaining elements.
   *
   *  $indexsignalling
   *  The index flag is initially set to maximum integer value.
   *
   *  @param pred   the predicate used to test the elements
   *  @return       a collection composed of all the elements after the longest prefix of elements
   *                in this $coll that satisfy the predicate `pred`
   */
  override def dropWhile(pred: T => Boolean): Repr = {
    val cntx = new DefaultSignalling with AtomicIndexFlag
    cntx.setIndexFlag(Int.MaxValue)
    executeAndWaitResult(new Span(0, pred, cbfactory, parallelIterator assign cntx) mapResult { _._2.result })
  }

  override def copyToArray[U >: T](xs: Array[U], start: Int, len: Int) = if (len > 0) {
    executeAndWait(new CopyToArray(start, len, xs, parallelIterator))
  }

  override def toIterable: Iterable[T] = seq.drop(0).asInstanceOf[Iterable[T]]

  override def toArray[U >: T: ClassManifest]: Array[U] = {
    val arr = new Array[U](size)
    copyToArray(arr)
    arr
  }

  override def toList: List[T] = seq.toList

  override def toIndexedSeq[S >: T]: collection.immutable.IndexedSeq[S] = seq.toIndexedSeq[S]

  override def toStream: Stream[T] = seq.toStream

  override def toSet[S >: T]: collection.immutable.Set[S] = seq.toSet

  override def toSeq: Seq[T] = seq.toSeq

  /* tasks */

  /** Standard accessor task that iterates over the elements of the collection.
   *
   *  @tparam R    type of the result of this method (`R` for result).
   *  @tparam Tp   the representation type of the task at hand.
   */
  protected trait Accessor[R, Tp]
  extends super.Task[R, Tp] {
    val pit: ParIterator
    def newSubtask(p: ParIterator): Accessor[R, Tp]
    def shouldSplitFurther = pit.remaining > threshold(size, parallelismLevel)
    def split = pit.split.map(newSubtask(_)) // default split procedure
    override def toString = "Accessor(" + pit.toString + ")"
  }

  protected[this] trait NonDivisibleTask[R, Tp] extends super.Task[R, Tp] {
    def shouldSplitFurther = false
    def split = throw new UnsupportedOperationException("Does not split.")
    override def toString = "NonDivisibleTask"
  }

  protected[this] trait NonDivisible[R] extends NonDivisibleTask[R, NonDivisible[R]]

  protected[this] trait Composite[FR, SR, R, First <: super.Task[FR, _], Second <: super.Task[SR, _]]
  extends NonDivisibleTask[R, Composite[FR, SR, R, First, Second]] {
    val ft: First
    val st: Second
    def combineResults(fr: FR, sr: SR): R
    var result: R = null.asInstanceOf[R]
  }

  /** Sequentially performs one task after another. */
  protected[this] trait SeqComposite[FR, SR, R, First <: super.Task[FR, _], Second <: super.Task[SR, _]]
  extends Composite[FR, SR, R, First, Second] {
    def leaf(prevr: Option[R]) = {
      ft.compute
      st.compute
      result = combineResults(ft.result, st.result)
    }
  }

  /** Performs two tasks in parallel, and waits for both to finish. */
  protected[this] trait ParComposite[FR, SR, R, First <: super.Task[FR, _], Second <: super.Task[SR, _]]
  extends Composite[FR, SR, R, First, Second] {
    def leaf(prevr: Option[R]) = {
      st.start
      ft.compute
      st.sync
      result = combineResults(ft.result, st.result)
    }
  }

  protected[this] abstract class ResultMapping[R, Tp, R1](val inner: Task[R, Tp])
  extends NonDivisibleTask[R1, ResultMapping[R, Tp, R1]] {
    var result: R1 = null.asInstanceOf[R1]
    def map(r: R): R1
    def leaf(prevr: Option[R1]) = {
      inner.compute
      result = map(inner.result)
    }
  }

  protected trait Transformer[R, Tp] extends Accessor[R, Tp]

  protected[this] class Foreach[S](op: T => S, val pit: ParIterator) extends Accessor[Unit, Foreach[S]] {
    var result: Unit = ()
    def leaf(prevr: Option[Unit]) = pit.foreach(op)
    def newSubtask(p: ParIterator) = new Foreach[S](op, p)
  }

  protected[this] class Count(pred: T => Boolean, val pit: ParIterator) extends Accessor[Int, Count] {
    var result: Int = 0
    def leaf(prevr: Option[Int]) = result = pit.count(pred)
    def newSubtask(p: ParIterator) = new Count(pred, p)
    override def merge(that: Count) = result = result + that.result
  }

  protected[this] class Reduce[U >: T](op: (U, U) => U, val pit: ParIterator) extends Accessor[U, Reduce[U]] {
    var result: U = null.asInstanceOf[U]
    def leaf(prevr: Option[U]) = result = pit.reduce(op)
    def newSubtask(p: ParIterator) = new Reduce(op, p)
    override def merge(that: Reduce[U]) = result = op(result, that.result)
  }

  protected[this] class Fold[U >: T](z: U, op: (U, U) => U, val pit: ParIterator) extends Accessor[U, Fold[U]] {
    var result: U = null.asInstanceOf[U]
    def leaf(prevr: Option[U]) = result = pit.fold(z)(op)
    def newSubtask(p: ParIterator) = new Fold(z, op, p)
    override def merge(that: Fold[U]) = result = op(result, that.result)
  }

  protected[this] class Aggregate[S](z: S, seqop: (S, T) => S, combop: (S, S) => S, val pit: ParIterator)
  extends Accessor[S, Aggregate[S]] {
    var result: S = null.asInstanceOf[S]
    def leaf(prevr: Option[S]) = result = pit.foldLeft(z)(seqop)
    def newSubtask(p: ParIterator) = new Aggregate(z, seqop, combop, p)
    override def merge(that: Aggregate[S]) = result = combop(result, that.result)
  }

  protected[this] class Sum[U >: T](num: Numeric[U], val pit: ParIterator) extends Accessor[U, Sum[U]] {
    var result: U = null.asInstanceOf[U]
    def leaf(prevr: Option[U]) = result = pit.sum(num)
    def newSubtask(p: ParIterator) = new Sum(num, p)
    override def merge(that: Sum[U]) = result = num.plus(result, that.result)
  }

  protected[this] class Product[U >: T](num: Numeric[U], val pit: ParIterator) extends Accessor[U, Product[U]] {
    var result: U = null.asInstanceOf[U]
    def leaf(prevr: Option[U]) = result = pit.product(num)
    def newSubtask(p: ParIterator) = new Product(num, p)
    override def merge(that: Product[U]) = result = num.times(result, that.result)
  }

  protected[this] class Min[U >: T](ord: Ordering[U], val pit: ParIterator) extends Accessor[U, Min[U]] {
    var result: U = null.asInstanceOf[U]
    def leaf(prevr: Option[U]) = result = pit.min(ord)
    def newSubtask(p: ParIterator) = new Min(ord, p)
    override def merge(that: Min[U]) = result = if (ord.lteq(result, that.result)) result else that.result
  }

  protected[this] class Max[U >: T](ord: Ordering[U], val pit: ParIterator) extends Accessor[U, Max[U]] {
    var result: U = null.asInstanceOf[U]
    def leaf(prevr: Option[U]) = result = pit.max(ord)
    def newSubtask(p: ParIterator) = new Max(ord, p)
    override def merge(that: Max[U]) = result = if (ord.gteq(result, that.result)) result else that.result
  }

  protected[this] class Map[S, That](f: T => S, pbf: CanCombineFrom[Repr, S, That], val pit: ParIterator)
  extends Transformer[Combiner[S, That], Map[S, That]] {
    var result: Combiner[S, That] = null
    def leaf(prev: Option[Combiner[S, That]]) = result = pit.map2combiner(f, reuse(prev, pbf(self.repr)))
    def newSubtask(p: ParIterator) = new Map(f, pbf, p)
    override def merge(that: Map[S, That]) = result = result combine that.result
  }

  protected[this] class Collect[S, That]
  (pf: PartialFunction[T, S], pbf: CanCombineFrom[Repr, S, That], val pit: ParIterator)
  extends Transformer[Combiner[S, That], Collect[S, That]] {
    var result: Combiner[S, That] = null
    def leaf(prev: Option[Combiner[S, That]]) = result = pit.collect2combiner[S, That](pf, pbf) // TODO
    def newSubtask(p: ParIterator) = new Collect(pf, pbf, p)
    override def merge(that: Collect[S, That]) = result = result combine that.result
  }

  protected[this] class FlatMap[S, That](f: T => Traversable[S], pbf: CanCombineFrom[Repr, S, That], val pit: ParIterator)
  extends Transformer[Combiner[S, That], FlatMap[S, That]] {
    var result: Combiner[S, That] = null
    def leaf(prev: Option[Combiner[S, That]]) = result = pit.flatmap2combiner(f, pbf) // TODO
    def newSubtask(p: ParIterator) = new FlatMap(f, pbf, p)
    override def merge(that: FlatMap[S, That]) = result = result combine that.result
  }

  protected[this] class Forall(pred: T => Boolean, val pit: ParIterator) extends Accessor[Boolean, Forall] {
    var result: Boolean = true
    def leaf(prev: Option[Boolean]) = { if (!pit.isAborted) result = pit.forall(pred); if (result == false) pit.abort }
    def newSubtask(p: ParIterator) = new Forall(pred, p)
    override def merge(that: Forall) = result = result && that.result
  }

  protected[this] class Exists(pred: T => Boolean, val pit: ParIterator) extends Accessor[Boolean, Exists] {
    var result: Boolean = false
    def leaf(prev: Option[Boolean]) = { if (!pit.isAborted) result = pit.exists(pred); if (result == true) pit.abort }
    def newSubtask(p: ParIterator) = new Exists(pred, p)
    override def merge(that: Exists) = result = result || that.result
  }

  protected[this] class Find[U >: T](pred: T => Boolean, val pit: ParIterator) extends Accessor[Option[U], Find[U]] {
    var result: Option[U] = None
    def leaf(prev: Option[Option[U]]) = { if (!pit.isAborted) result = pit.find(pred); if (result != None) pit.abort }
    def newSubtask(p: ParIterator) = new Find(pred, p)
    override def merge(that: Find[U]) = if (this.result == None) result = that.result
  }

  protected[this] class Filter[U >: T, This >: Repr](pred: T => Boolean, cbf: () => Combiner[U, This], val pit: ParIterator)
  extends Transformer[Combiner[U, This], Filter[U, This]] {
    var result: Combiner[U, This] = null
    def leaf(prev: Option[Combiner[U, This]]) = result = pit.filter2combiner(pred, reuse(prev, cbf()))
    def newSubtask(p: ParIterator) = new Filter(pred, cbf, p)
    override def merge(that: Filter[U, This]) = result = result combine that.result
  }

  protected[this] class FilterNot[U >: T, This >: Repr](pred: T => Boolean, cbf: () => Combiner[U, This], val pit: ParIterator)
  extends Transformer[Combiner[U, This], FilterNot[U, This]] {
    var result: Combiner[U, This] = null
    def leaf(prev: Option[Combiner[U, This]]) = result = pit.filterNot2combiner(pred, reuse(prev, cbf()))
    def newSubtask(p: ParIterator) = new FilterNot(pred, cbf, p)
    override def merge(that: FilterNot[U, This]) = result = result combine that.result
  }

  protected class Copy[U >: T, That](cfactory: () => Combiner[U, That], val pit: ParIterator)
  extends Transformer[Combiner[U, That], Copy[U, That]] {
    var result: Combiner[U, That] = null
    def leaf(prev: Option[Combiner[U, That]]) = result = pit.copy2builder[U, That, Combiner[U, That]](reuse(prev, cfactory()))
    def newSubtask(p: ParIterator) = new Copy[U, That](cfactory, p)
    override def merge(that: Copy[U, That]) = result = result combine that.result
  }

  protected[this] class Partition[U >: T, This >: Repr](pred: T => Boolean, cbf: () => Combiner[U, This], val pit: ParIterator)
  extends Transformer[(Combiner[U, This], Combiner[U, This]), Partition[U, This]] {
    var result: (Combiner[U, This], Combiner[U, This]) = null
    def leaf(prev: Option[(Combiner[U, This], Combiner[U, This])]) = result = pit.partition2combiners(pred, reuse(prev.map(_._1), cbf()), reuse(prev.map(_._2), cbf()))
    def newSubtask(p: ParIterator) = new Partition(pred, cbf, p)
    override def merge(that: Partition[U, This]) = result = (result._1 combine that.result._1, result._2 combine that.result._2)
  }

  protected[this] class Take[U >: T, This >: Repr](n: Int, cbf: () => Combiner[U, This], val pit: ParIterator)
  extends Transformer[Combiner[U, This], Take[U, This]] {
    var result: Combiner[U, This] = null
    def leaf(prev: Option[Combiner[U, This]]) = result = pit.take2combiner(n, reuse(prev, cbf()))
    def newSubtask(p: ParIterator) = throw new UnsupportedOperationException
    override def split = {
      val pits = pit.split
      val sizes = pits.scanLeft(0)(_ + _.remaining)
      for ((p, untilp) <- pits zip sizes; if untilp <= n) yield {
        if (untilp + p.remaining < n) new Take(p.remaining, cbf, p)
        else new Take(n - untilp, cbf, p)
      }
    }
    override def merge(that: Take[U, This]) = result = result combine that.result
  }

  protected[this] class Drop[U >: T, This >: Repr](n: Int, cbf: () => Combiner[U, This], val pit: ParIterator)
  extends Transformer[Combiner[U, This], Drop[U, This]] {
    var result: Combiner[U, This] = null
    def leaf(prev: Option[Combiner[U, This]]) = result = pit.drop2combiner(n, reuse(prev, cbf()))
    def newSubtask(p: ParIterator) = throw new UnsupportedOperationException
    override def split = {
      val pits = pit.split
      val sizes = pits.scanLeft(0)(_ + _.remaining)
      for ((p, withp) <- pits zip sizes.tail; if withp >= n) yield {
        if (withp - p.remaining > n) new Drop(0, cbf, p)
        else new Drop(n - withp + p.remaining, cbf, p)
      }
    }
    override def merge(that: Drop[U, This]) = result = result combine that.result
  }

  protected[this] class Slice[U >: T, This >: Repr](from: Int, until: Int, cbf: () => Combiner[U, This], val pit: ParIterator)
  extends Transformer[Combiner[U, This], Slice[U, This]] {
    var result: Combiner[U, This] = null
    def leaf(prev: Option[Combiner[U, This]]) = result = pit.slice2combiner(from, until, reuse(prev, cbf()))
    def newSubtask(p: ParIterator) = throw new UnsupportedOperationException
    override def split = {
      val pits = pit.split
      val sizes = pits.scanLeft(0)(_ + _.remaining)
      for ((p, untilp) <- pits zip sizes; if untilp + p.remaining >= from || untilp <= until) yield {
        val f = (from max untilp) - untilp
        val u = (until min (untilp + p.remaining)) - untilp
        new Slice(f, u, cbf, p)
      }
    }
    override def merge(that: Slice[U, This]) = result = result combine that.result
  }

  protected[this] class SplitAt[U >: T, This >: Repr](at: Int, cbf: () => Combiner[U, This], val pit: ParIterator)
  extends Transformer[(Combiner[U, This], Combiner[U, This]), SplitAt[U, This]] {
    var result: (Combiner[U, This], Combiner[U, This]) = null
    def leaf(prev: Option[(Combiner[U, This], Combiner[U, This])]) = result = pit.splitAt2combiners(at, reuse(prev.map(_._1), cbf()), reuse(prev.map(_._2), cbf()))
    def newSubtask(p: ParIterator) = throw new UnsupportedOperationException
    override def split = {
      val pits = pit.split
      val sizes = pits.scanLeft(0)(_ + _.remaining)
      for ((p, untilp) <- pits zip sizes) yield new SplitAt((at max untilp min (untilp + p.remaining)) - untilp, cbf, p)
    }
    override def merge(that: SplitAt[U, This]) = result = (result._1 combine that.result._1, result._2 combine that.result._2)
  }

  protected[this] class TakeWhile[U >: T, This >: Repr]
  (pos: Int, pred: T => Boolean, cbf: () => Combiner[U, This], val pit: ParIterator)
  extends Transformer[(Combiner[U, This], Boolean), TakeWhile[U, This]] {
    var result: (Combiner[U, This], Boolean) = null
    def leaf(prev: Option[(Combiner[U, This], Boolean)]) = if (pos < pit.indexFlag) {
      result = pit.takeWhile2combiner(pred, reuse(prev.map(_._1), cbf()))
      if (!result._2) pit.setIndexFlagIfLesser(pos)
    } else result = (reuse(prev.map(_._1), cbf()), false)
    def newSubtask(p: ParIterator) = throw new UnsupportedOperationException
    override def split = {
      val pits = pit.split
      for ((p, untilp) <- pits zip pits.scanLeft(0)(_ + _.remaining)) yield new TakeWhile(pos + untilp, pred, cbf, p)
    }
    override def merge(that: TakeWhile[U, This]) = if (result._2) {
      result = (result._1 combine that.result._1, that.result._2)
    }
  }

  protected[this] class Span[U >: T, This >: Repr]
  (pos: Int, pred: T => Boolean, cbf: () => Combiner[U, This], val pit: ParIterator)
  extends Transformer[(Combiner[U, This], Combiner[U, This]), Span[U, This]] {
    var result: (Combiner[U, This], Combiner[U, This]) = null
    def leaf(prev: Option[(Combiner[U, This], Combiner[U, This])]) = if (pos < pit.indexFlag) {
      result = pit.span2combiners(pred, reuse(prev.map(_._1), cbf()), reuse(prev.map(_._2), cbf()))
      if (result._2.size > 0) pit.setIndexFlagIfLesser(pos)
    } else {
      result = (reuse(prev.map(_._2), cbf()), pit.copy2builder[U, This, Combiner[U, This]](reuse(prev.map(_._2), cbf())))
    }
    def newSubtask(p: ParIterator) = throw new UnsupportedOperationException
    override def split = {
      val pits = pit.split
      for ((p, untilp) <- pits zip pits.scanLeft(0)(_ + _.remaining)) yield new Span(pos + untilp, pred, cbf, p)
    }
    override def merge(that: Span[U, This]) = result = if (result._2.size == 0) {
      (result._1 combine that.result._1, that.result._2)
    } else {
      (result._1, result._2 combine that.result._1 combine that.result._2)
    }
  }

  protected[this] class CopyToArray[U >: T, This >: Repr](from: Int, len: Int, array: Array[U], val pit: ParIterator)
  extends Accessor[Unit, CopyToArray[U, This]] {
    var result: Unit = ()
    def leaf(prev: Option[Unit]) = pit.copyToArray(array, from, len)
    def newSubtask(p: ParIterator) = unsupported
    override def split = {
      val pits = pit.split
      for ((p, untilp) <- pits zip pits.scanLeft(0)(_ + _.remaining); if untilp < len) yield {
        val plen = p.remaining min (len - untilp)
        new CopyToArray[U, This](from + untilp, plen, array, p)
      }
    }
  }

  protected[this] class ScanToArray[U >: T, A >: U](z: U, op: (U, U) => U, val from: Int, val len: Int, array: Array[A], val pit: ParIterator)
  extends Accessor[Boolean, ScanToArray[U, A]] {
    var result: Boolean = false // whether it was prefix-scanned, because previous result was already available
    def leaf(prev: Option[Boolean]) = if (prev.isDefined) { // use prev result as an optimisation
      val lastelem = array(from - 1)
      pit.scanToArray(lastelem.asInstanceOf[U], op, array, from)
      result = true
    } else pit.scanToArray(z, op, array, from)
    def newSubtask(p: ParIterator) = unsupported
    override lazy val shouldSplitFurther = {
      // we want less work stealings while prefix scanning
      // and keep processors busier with merging
      // (work stealing while prefix scanning means more work later)
      val processors = parallelismLevel min availableProcessors
      len > (((size + 1) / tweak(processors)) max 1)
    }
    private def tweak(p: Int) = 2 // if (p < 4) p else p / 2
    override def split = {
      val pits = pit.split
      for ((p, untilp) <- pits zip pits.scanLeft(0)(_ + _.remaining); if untilp < len) yield {
        val plen = p.remaining min (len - untilp)
        new ScanToArray[U, A](z, op, from + untilp, plen, array, p)
      }
    }
    override def merge(that: ScanToArray[U, A]) = if (!that.result) { // if previous result wasn't available when task was initiated
      // apply the rightmost element of this array part to all the elements of `that`
      executeAndWait(new ApplyToArray(array(that.from - 1).asInstanceOf[U], op, that.from, that.len, array))
    }
  }

  protected[this] class ApplyToArray[U >: T, A >: U](elem: U, op: (U, U) => U, from: Int, len: Int, array: Array[A])
  extends super.Task[Unit, ApplyToArray[U, A]] {
    var result: Unit = ()
    def leaf(prev: Option[Unit]) = {
      var i = from
      val until = from + len
      while (i < until) {
        array(i) = op(elem, array(i).asInstanceOf[U])
        i += 1
      }
    }
    def shouldSplitFurther = len > (parallelismLevel min availableProcessors) // threshold(size, parallelismLevel)
    def split = {
      val fp = len / 2
      val sp = len - fp
      Seq(
        new ApplyToArray(elem, op, from, fp, array),
        new ApplyToArray(elem, op, from + fp, sp, array)
      )
    }
  }

  protected[this] class FromArray[S, A, That](array: Array[A], from: Int, len: Int, cbf: CanCombineFrom[Repr, S, That])
  extends super.Task[Combiner[S, That], FromArray[S, A, That]] {
    var result: Result = null
    def leaf(prev: Option[Result]) = {
      val cb = prev getOrElse cbf(self.repr)
      var i = from
      val until = from + len
      while (i < until) {
        cb += array(i).asInstanceOf[S]
        i += 1
      }
      result = cb
    }
    def shouldSplitFurther = len > threshold(size, parallelismLevel)
    def split = {
      val fp = len / 2
      val sp = len - fp
      Seq(
        new FromArray(array, from, fp, cbf),
        new FromArray(array, from + fp, sp, cbf)
      )
    }
    override def merge(that: FromArray[S, A, That]) = result = result combine that.result
  }

}




























