package strawman.collections

import Predef.{augmentString => _, wrapString => _, _}
import scala.reflect.ClassTag
import annotation.unchecked.uncheckedVariance
import annotation.tailrec

class LowPriority {
  import CollectionStrawMan6._

  /** Convert array to iterable via view. Lower priority than ArrayOps */
  implicit def arrayToView[T](xs: Array[T]): ArrayView[T] = new ArrayView[T](xs)

  /** Convert string to iterable via view. Lower priority than StringOps */
  implicit def stringToView(s: String): StringView = new StringView(s)
}

/** A strawman architecture for new collections. It contains some
 *  example collection classes and methods with the intent to expose
 *  some key issues. It would be good to compare this to odether
 *  implementations of the same functionality, to get an idea of the
 *  strengths and weaknesses of different collection architectures.
 *
 *  For a test file, see tests/run/CollectionTests.scala.
 *
 *  Strawman6 is like strawman5, and adds lazy lists (i.e. lazie streams), arrays
 *  and some utilitity methods (take, tail, mkString, toArray). Also, systematically
 *  uses builders for all strict collections.
 *
 *  Types covered in this strawman:
 *
 *  1. Collection base types:
 *
 *         IterableOnce, Iterable, Seq, LinearSeq, View, IndexedView
 *
 *  2. Collection creator base types:
 *
 *         FromIterable, IterableFactory, Buildable, Builder
 *
 *  3. Types that bundle operations:
 *
 *         IterableOps, IterableMonoTransforms, IterablePolyTransforms, IterableLike
 *         SeqMonoTransforms, SeqLike
 *
 *  4. Concrete collection types:
 *
 *         List, LazyList, ListBuffer, ArrayBuffer, ArrayBufferView, StringView, ArrayView
 *
 *  5. Decorators for existing types
 *
 *         StringOps, ArrayOps
 *
 *  6. Related non collection types:
 *
 *         Iterator, StringBuilder
 *
 *  Operations covered in this strawman:
 *
 *   1. Abstract operations, or expected to be overridden:
 *
 *      For iterables:
 *
 *         iterator, fromIterable, fromIterableWithSameElemType, knownLength, className
 *
 *      For sequences:
 *
 *         apply, length
 *
 *      For buildables:
 *
 *         newBuilder
 *
 *      For builders:
 *
 *         +=, result
 *
 *   2. Utility methods, might be overridden for performance:
 *
 *      Operations returning not necessarily a collection:
 *
 *         foreach, foldLeft, foldRight, indexWhere, isEmpty, head, size, mkString
 *
 *      Operations returning a collection of a fixed type constructor:
 *
 *         view, to, toArray, copyToArray
 *
 *      Type-preserving generic transforms:
 *
 *         filter, partition, take, drop, tail, reverse
 *
 *      Generic transforms returning collections of different element types:
 *
 *         map, flatMap, ++, zip
 */
object CollectionStrawMan6 extends LowPriority {

  /* ------------ Base Traits -------------------------------- */

  /** Iterator can be used only once */
  trait IterableOnce[+A] {
    def iterator: Iterator[A]
  }

  /** Base trait for instances that can construct a collection from an iterable */
  trait FromIterable[+C[X] <: Iterable[X]] {
    def fromIterable[B](it: Iterable[B]): C[B]
  }

  /** Base trait for companion objects of collections */
  trait IterableFactory[+C[X] <: Iterable[X]] extends FromIterable[C] {
    def empty[X]: C[X] = fromIterable(View.Empty)
    def apply[A](xs: A*): C[A] = fromIterable(View.Elems(xs: _*))
  }

  /** Base trait for generic collections */
  trait Iterable[+A] extends IterableOnce[A] with IterableLike[A, Iterable] {
    /** The collection itself */
    protected def coll: this.type = this
  }

  /** A trait representing indexable collections with finite length */
  trait ArrayLike[+A] extends Any {
    def length: Int
    def apply(i: Int): A
  }

  /** Base trait for sequence collections */
  trait Seq[+A] extends Iterable[A] with SeqLike[A, Seq] with ArrayLike[A]

  /** Base trait for linearly accessed sequences that have efficient `head` and
   *  `tail` operations.
   *  Known subclasses: List, LazyList
   */
  trait LinearSeq[+A] extends Seq[A] with LinearSeqLike[A, LinearSeq] { self =>

    /** To be overridden in implementations: */
    def isEmpty: Boolean
    def head: A
    def tail: LinearSeq[A]

    /** `iterator` is overridden in terms of `head` and `tail` */
    def iterator = new Iterator[A] {
      private[this] var current: Seq[A] = self
      def hasNext = !current.isEmpty
      def next = { val r = current.head; current = current.tail; r }
    }

    /** `length is defined in terms of `iterator` */
    def length: Int = iterator.length

    /** `apply` is defined in terms of `drop`, which is in turn defined in
     *  terms of `tail`.
     */
    override def apply(n: Int): A = {
      if (n < 0) throw new IndexOutOfBoundsException(n.toString)
      val skipped = drop(n)
      if (skipped.isEmpty) throw new IndexOutOfBoundsException(n.toString)
      skipped.head
    }
  }

  type IndexedSeq[+A] = Seq[A] { def view: IndexedView[A] }

  /** Base trait for strict collections that can be built using a builder.
   *  @param  A    the element type of the collection
   *  @param Repr  the type of the underlying collection
   */
  trait Buildable[+A, +Repr] extends Any with IterableMonoTransforms[A, Repr]  {

    /** Creates a new builder. */
    protected[this] def newBuilder: Builder[A, Repr]

    /** Optimized, push-based version of `partition`. */
    override def partition(p: A => Boolean): (Repr, Repr) = {
      val l, r = newBuilder
      coll.iterator.foreach(x => (if (p(x)) l else r) += x)
      (l.result, r.result)
    }

    // one might also override other transforms here to avoid generating
    // iterators if it helps efficiency.
  }

  /** Base trait for collection builders */
  trait Builder[-A, +To] { self =>

    /** Append an element */
    def +=(x: A): this.type

    /** Result collection consisting of all elements appended so far. */
    def result: To

    /** Bulk append. Can be overridden if specialized implementations are available. */
    def ++=(xs: IterableOnce[A]): this.type = {
      xs.iterator.foreach(+=)
      this
    }

    /** A builder resulting from this builder my mapping the result using `f`. */
    def mapResult[NewTo](f: To => NewTo) = new Builder[A, NewTo] {
      def +=(x: A): this.type = { self += x; this }
      override def ++=(xs: IterableOnce[A]): this.type = { self ++= xs; this }
      def result: NewTo = f(self.result)
    }
  }

  /* ------------ Operations ----------------------------------- */

  /** Base trait for Iterable operations
   *
   *  VarianceNote
   *  ============
   *
   *  We require that for all child classes of Iterable the variance of
   *  the child class and the variance of the `C` parameter passed to `IterableLike`
   *  are the same. We cannot express this since we lack variance polymorphism. That's
   *  why we have to resort at some places to write `C[A @uncheckedVariance]`.
   *
   */
  trait IterableLike[+A, +C[X] <: Iterable[X]]
    extends FromIterable[C]
       with IterableOps[A]
       with IterableMonoTransforms[A, C[A @uncheckedVariance]] // sound bcs of VarianceNote
       with IterablePolyTransforms[A, C] {

    /** Create a collection of type `C[A]` from the elements of `coll`, which has
     *  the same element type as this collection. Overridden in StringOps and ArrayOps.
     */
    protected[this] def fromIterableWithSameElemType(coll: Iterable[A]): C[A] = fromIterable(coll)
  }

  /** Base trait for Seq operations */
  trait SeqLike[+A, +C[X] <: Seq[X]]
  extends IterableLike[A, C]
     with SeqMonoTransforms[A, C[A @uncheckedVariance]] // sound bcs of VarianceNote

  /** Base trait for linear Seq operations */
  trait LinearSeqLike[+A, +C[X] <: LinearSeq[X]] extends SeqLike[A, C] {

    /** Optimized version of `drop` that avoids copying
     *  Note: `drop` is defined here, rather than in a trait like `LinearSeqMonoTransforms`,
     *  because the `...MonoTransforms` traits make no assumption about the type of `Repr`
     *  whereas we need to assume here that `Repr` is the same as the underlying
     *  collection type.
     */
    override def drop(n: Int): C[A @uncheckedVariance] = { // sound bcs of VarianceNote
      def loop(n: Int, s: Iterable[A]): C[A] =
        if (n <= 0) s.asInstanceOf[C[A]]
           // implicit contract to guarantee success of asInstanceOf:
           //   (1) coll is of type C[A]
           //   (2) The tail of a LinearSeq is of the same type as the type of the sequence itself
           // it's surprisingly tricky/ugly to turn this into actual types, so we
           // leave this contract implicit.
        else loop(n - 1, s.tail)
      loop(n, coll)
    }
  }

  /** Operations over iterables. No operation defined here is generic in the
   *  type of the underlying collection.
   */
  trait IterableOps[+A] extends Any {
    protected def coll: Iterable[A]
    private def iterator = coll.iterator

    /** Apply `f` to each element for tis side effects */
    def foreach(f: A => Unit): Unit = iterator.foreach(f)

    /** Fold left */
    def foldLeft[B](z: B)(op: (B, A) => B): B = iterator.foldLeft(z)(op)

    /** Fold right */
    def foldRight[B](z: B)(op: (A, B) => B): B = iterator.foldRight(z)(op)

    /** The index of the first element in this collection for which `p` holds. */
    def indexWhere(p: A => Boolean): Int = iterator.indexWhere(p)

    /** Is the collection empty? */
    def isEmpty: Boolean = !iterator.hasNext

    /** The first element of the collection. */
    def head: A = iterator.next()

    /** The number of elements in this collection, if it can be cheaply computed,
     *  -1 otherwise. Cheaply usually means: Not requiring a collection traversal.
     */
    def knownSize: Int = -1

    /** The number of elements in this collection. Does not terminate for
     *  infinite collections.
     */
    def size: Int = if (knownSize >= 0) knownSize else iterator.length

    /** A view representing the elements of this collection. */
    def view: View[A] = View.fromIterator(iterator)

     /** Given a collection factory `fi` for collections of type constructor `C`,
     *  convert this collection to one of type `C[A]`. Example uses:
     *
     *      xs.to(List)
     *      xs.to(ArrayBuffer)
     */
    def to[C[X] <: Iterable[X]](fi: FromIterable[C]): C[A @uncheckedVariance] =
      // variance seems sound because `to` could just as well have been added
      // as a decorator. We should investigate this further to be sure.
      fi.fromIterable(coll)

    /** Convert collection to array. */
    def toArray[B >: A: ClassTag]: Array[B] =
      if (knownSize >= 0) copyToArray(new Array[B](knownSize), 0)
      else ArrayBuffer.fromIterable(coll).toArray[B]

    /** Copy all elements of this collection to array `xs`, starting at `start`. */
    def copyToArray[B >: A](xs: Array[B], start: Int = 0): xs.type = {
      var i = start
      val it = iterator
      while (it.hasNext) {
        xs(i) = it.next()
        i += 1
      }
      xs
    }

    /** The class name of this collection. To be used for converting to string.
     *  Collections generally print like this:
     *
     *       <className>(elem_1, ..., elem_n)
     */
    def className = getClass.getName

    /** A string showing all elements of this collection, separated by string `sep`. */
    def mkString(sep: String): String = {
      var first: Boolean = true
      val b = new StringBuilder()
      foreach { elem =>
        if (!first) b ++= sep
        first = false
        b ++= String.valueOf(elem)
      }
      b.result
    }

    override def toString = s"$className(${mkString(", ")})"
  }

  /** Type-preserving transforms over iterables.
   *  Operations defined here return in their result iterables of the same type
   *  as the one they are invoked on.
   */
  trait IterableMonoTransforms[+A, +Repr] extends Any {
    protected def coll: Iterable[A]
    protected[this] def fromIterableWithSameElemType(coll: Iterable[A]): Repr

    /** All elements satisfying predicate `p` */
    def filter(p: A => Boolean): Repr = fromIterableWithSameElemType(View.Filter(coll, p))

    /** A pair of, first, all elements that satisfy prediacte `p` and, second,
     *  all elements that do not. Interesting because it splits a collection in two.
     *
     *  The default implementation provided here needs to traverse the collection twice.
     *  Strict collections have an overridden version of `partition` in `Buildable`,
     *  which requires only a single traversal.
     */
    def partition(p: A => Boolean): (Repr, Repr) = {
      val pn = View.Partition(coll, p)
      (fromIterableWithSameElemType(pn.left), fromIterableWithSameElemType(pn.right))
    }

    /** A collection containing the first `n` elements of this collection. */
    def take(n: Int): Repr = fromIterableWithSameElemType(View.Take(coll, n))

    /** The rest of the collection without its `n` first elements. For
     *  linear, immutable collections this should avoid making a copy.
     */
    def drop(n: Int): Repr = fromIterableWithSameElemType(View.Drop(coll, n))

    /** The rest of the collection without its first element. */
    def tail: Repr = drop(1)
  }

  /** Transforms over iterables that can return collections of different element types.
   */
  trait IterablePolyTransforms[+A, +C[A]] extends Any {
    protected def coll: Iterable[A]
    def fromIterable[B](coll: Iterable[B]): C[B]

    /** Map */
    def map[B](f: A => B): C[B] = fromIterable(View.Map(coll, f))

    /** Flatmap */
    def flatMap[B](f: A => IterableOnce[B]): C[B] = fromIterable(View.FlatMap(coll, f))

    /** Concatenation */
    def ++[B >: A](xs: IterableOnce[B]): C[B] = fromIterable(View.Concat(coll, xs))

    /** Zip. Interesting because it requires to align to source collections. */
    def zip[B](xs: IterableOnce[B]): C[(A @uncheckedVariance, B)] = fromIterable(View.Zip(coll, xs))
       // sound bcs of VarianceNote
  }

  /** Type-preserving transforms over sequences. */
  trait SeqMonoTransforms[+A, +Repr] extends Any with IterableMonoTransforms[A, Repr] {
    def reverse: Repr = coll.view match {
      case v: IndexedView[A] => fromIterableWithSameElemType(v.reverse)
      case _ =>
        var xs: List[A] = Nil
        var it = coll.iterator
        while (it.hasNext) xs = it.next() :: xs
        fromIterableWithSameElemType(xs)
    }
  }

  /* --------- Concrete collection types ------------------------------- */

  /** Concrete collection type: List */
  sealed trait List[+A]
  extends LinearSeq[A]
     with SeqLike[A, List]
     with Buildable[A, List[A]] {

    def fromIterable[B](c: Iterable[B]): List[B] = List.fromIterable(c)

    protected[this] def newBuilder = new ListBuffer[A].mapResult(_.toList)

    /** Prepend element */
    def :: [B >: A](elem: B): List[B] =  new ::(elem, this)

    /** Prepend operation that avoids copying this list */
    def ++:[B >: A](prefix: List[B]): List[B] =
      if (prefix.isEmpty) this
      else prefix.head :: prefix.tail ++: this

    /** When concatenating with another list `xs`, avoid copying `xs` */
    override def ++[B >: A](xs: IterableOnce[B]): List[B] = xs match {
      case xs: List[B] => this ++: xs
      case _ => super.++(xs)
    }

    override def className = "List"
  }

  case class :: [+A](x: A, private[collections] var next: List[A @uncheckedVariance]) // sound because `next` is used only locally
  extends List[A] {
    override def isEmpty = false
    override def head = x
    override def tail = next
  }

  case object Nil extends List[Nothing] {
    override def isEmpty = true
    override def head = ???
    override def tail = ???
  }

  object List extends IterableFactory[List] {
    def fromIterable[B](coll: Iterable[B]): List[B] = coll match {
      case coll: List[B] => coll
      case _ => ListBuffer.fromIterable(coll).toList
    }
  }

  /** Concrete collection type: ListBuffer */
  class ListBuffer[A]
  extends Seq[A]
     with SeqLike[A, ListBuffer]
     with Buildable[A, ListBuffer[A]]
     with Builder[A, ListBuffer[A]] {

    private var first, last: List[A] = Nil
    private var aliased = false
    private var len = 0

    def iterator = first.iterator

    def fromIterable[B](coll: Iterable[B]) = ListBuffer.fromIterable(coll)

    def apply(i: Int) = first.apply(i)

    def length = len
    override def knownSize = len

    protected[this] def newBuilder = new ListBuffer[A]

    private def copyElems(): Unit = {
      val buf = ListBuffer.fromIterable(result)
      first = buf.first
      last = buf.last
      aliased = false
    }

    /** Convert to list; avoids copying where possible. */
    def toList = {
      aliased = true
      first
    }

    def +=(elem: A) = {
      if (aliased) copyElems()
      val last1 = elem :: Nil
      last match {
        case last: ::[A] => last.next = last1
        case _ => first = last1
      }
      last = last1
      len += 1
      this
    }

    def result = this

    override def className = "ListBuffer"
  }

  object ListBuffer extends IterableFactory[ListBuffer] {
    def fromIterable[B](coll: Iterable[B]): ListBuffer[B] = new ListBuffer[B] ++= coll
  }

  /** Concrete collection type: ArrayBuffer */
  class ArrayBuffer[A] private (initElems: Array[AnyRef], initLength: Int)
  extends Seq[A]
     with SeqLike[A, ArrayBuffer]
     with Buildable[A, ArrayBuffer[A]]
     with Builder[A, ArrayBuffer[A]] {

    def this() = this(new Array[AnyRef](16), 0)

    private var elems: Array[AnyRef] = initElems
    private var start = 0
    private var end = initLength

    def apply(n: Int) = elems(start + n).asInstanceOf[A]

    def length = end - start
    override def knownSize = length

    override def view = new ArrayBufferView(elems, start, end)

    def iterator = view.iterator

    def fromIterable[B](it: Iterable[B]): ArrayBuffer[B] =
      ArrayBuffer.fromIterable(it)

    protected[this] def newBuilder = new ArrayBuffer[A]

    def +=(elem: A): this.type = {
      if (end == elems.length) {
        if (start > 0) {
          Array.copy(elems, start, elems, 0, length)
          end -= start
          start = 0
        }
        else {
          val newelems = new Array[AnyRef](end * 2)
          Array.copy(elems, 0, newelems, 0, end)
          elems = newelems
        }
      }
      elems(end) = elem.asInstanceOf[AnyRef]
      end += 1
      this
    }

    def result = this

    /** New operation: destructively drop elements at start of buffer. */
    def trimStart(n: Int): Unit = start += (n max 0)

    /** Overridden to use array copying for efficiency where possible. */
    override def ++[B >: A](xs: IterableOnce[B]): ArrayBuffer[B] = xs match {
      case xs: ArrayBuffer[B] =>
        val elems = new Array[AnyRef](length + xs.length)
        Array.copy(this.elems, this.start, elems, 0, this.length)
        Array.copy(xs.elems, xs.start, elems, this.length, xs.length)
        new ArrayBuffer(elems, elems.length)
      case _ => super.++(xs)
    }

    override def take(n: Int) = {
      val elems = new Array[AnyRef](n min length)
      Array.copy(this.elems, this.start, elems, 0, elems.length)
      new ArrayBuffer(elems, elems.length)
    }

    override def className = "ArrayBuffer"
  }

  object ArrayBuffer extends IterableFactory[ArrayBuffer] {

    /** Avoid reallocation of buffer if length is known. */
    def fromIterable[B](coll: Iterable[B]): ArrayBuffer[B] =
      if (coll.knownSize >= 0) {
        val elems = new Array[AnyRef](coll.knownSize)
        val it = coll.iterator
        for (i <- 0 until elems.length) elems(i) = it.next().asInstanceOf[AnyRef]
        new ArrayBuffer[B](elems, elems.length)
      }
      else new ArrayBuffer[B] ++= coll
  }

  class ArrayBufferView[A](val elems: Array[AnyRef], val start: Int, val end: Int) extends IndexedView[A] {
    def length = end - start
    def apply(n: Int) = elems(start + n).asInstanceOf[A]
    override def className = "ArrayBufferView"
  }

  class LazyList[+A](expr: => LazyList.Evaluated[A])
  extends LinearSeq[A] with SeqLike[A, LazyList] {
    private[this] var evaluated = false
    private[this] var result: LazyList.Evaluated[A] = _

    def force: LazyList.Evaluated[A] = {
      if (!evaluated) {
        result = expr
        evaluated = true
      }
      result
    }

    override def isEmpty = force.isEmpty
    override def head = force.get._1
    override def tail = force.get._2

    def #:: [B >: A](elem: => B): LazyList[B] = new LazyList(Some((elem, this)))

    def fromIterable[B](c: Iterable[B]): LazyList[B] = LazyList.fromIterable(c)

    override def className = "LazyList"

    override def toString =
      if (evaluated)
        result match {
          case None => "Empty"
          case Some((hd, tl)) => s"$hd #:: $tl"
        }
      else "LazyList(?)"
  }

  object LazyList extends IterableFactory[LazyList] {

    type Evaluated[+A] = Option[(A, LazyList[A])]

    object Empty extends LazyList[Nothing](None)

    object #:: {
      def unapply[A](s: LazyList[A]): Evaluated[A] = s.force
    }

    def fromIterable[B](coll: Iterable[B]): LazyList[B] = coll match {
      case coll: LazyList[B] => coll
      case _ => fromIterator(coll.iterator)
    }

    def fromIterator[B](it: Iterator[B]): LazyList[B] =
      new LazyList(if (it.hasNext) Some(it.next(), fromIterator(it)) else None)
  }

  // ------------------ Decorators to add collection ops to existing types -----------------------

  /** Decorator to add collection operations to strings.
   */
  implicit class StringOps(val s: String)
  extends AnyVal with IterableOps[Char]
     with SeqMonoTransforms[Char, String]
     with IterablePolyTransforms[Char, List]
     with Buildable[Char, String]
     with ArrayLike[Char] {

    protected def coll = new StringView(s)
    def iterator = coll.iterator

    protected def fromIterableWithSameElemType(coll: Iterable[Char]): String = {
      val sb = new StringBuilder
      for (ch <- coll) sb += ch
      sb.result
    }

    def fromIterable[B](coll: Iterable[B]): List[B] = List.fromIterable(coll)

    protected[this] def newBuilder = new StringBuilder

    def length = s.length
    def apply(i: Int) = s.charAt(i)

    override def knownSize = s.length

    override def className = "String"

    /** Overloaded version of `map` that gives back a string, where the inherited
     *  version gives back a sequence.
     */
    def map(f: Char => Char): String = {
      val sb = new StringBuilder
      for (ch <- s) sb += f(ch)
      sb.result
    }

    /** Overloaded version of `flatMap` that gives back a string, where the inherited
     *  version gives back a sequence.
     */
    def flatMap(f: Char => String): String = {
      val sb = new StringBuilder
      for (ch <- s) sb ++= f(ch)
      sb.result
    }

    /** Overloaded version of `++` that gives back a string, where the inherited
     *  version gives back a sequence.
     */
    def ++(xs: IterableOnce[Char]): String = {
      val sb = new StringBuilder() ++= s
      for (ch <- xs.iterator) sb += ch
      sb.result
    }

    /** Another overloaded version of `++`. */
    def ++(xs: String): String = s + xs
  }

  class StringBuilder extends Builder[Char, String] {
    private val sb = new java.lang.StringBuilder

    def += (x: Char) = { sb.append(x); this }

    /** Overloaded version of `++=` that takes a string */
    def ++= (s: String) = { sb.append(s); this }

    def result = sb.toString

    override def toString = result
  }

  case class StringView(s: String) extends IndexedView[Char] {
    def length = s.length
    def apply(n: Int) = s.charAt(n)
    override def className = "StringView"
  }

  /** Decorator to add collection operations to arrays.
   */
  implicit class ArrayOps[A](val xs: Array[A])
  extends AnyVal with IterableOps[A]
     with SeqMonoTransforms[A, Array[A]]
     with Buildable[A, Array[A]]
     with ArrayLike[A] {

    protected def coll = new ArrayView(xs)
    def iterator = coll.iterator

    def length = xs.length
    def apply(i: Int) = xs.apply(i)

    override def view = new ArrayView(xs)

    def elemTag: ClassTag[A] = ClassTag(xs.getClass.getComponentType)

    protected def fromIterableWithSameElemType(coll: Iterable[A]): Array[A] = coll.toArray[A](elemTag)

    def fromIterable[B: ClassTag](coll: Iterable[B]): Array[B] = coll.toArray[B]

    protected[this] def newBuilder = new ArrayBuffer[A].mapResult(_.toArray(elemTag))

    override def knownSize = xs.length

    override def className = "Array"

    def map[B: ClassTag](f: A => B): Array[B] = fromIterable(View.Map(coll, f))
    def flatMap[B: ClassTag](f: A => IterableOnce[B]): Array[B] = fromIterable(View.FlatMap(coll, f))
    def ++[B >: A : ClassTag](xs: IterableOnce[B]): Array[B] = fromIterable(View.Concat(coll, xs))
    def zip[B: ClassTag](xs: IterableOnce[B]): Array[(A, B)] = fromIterable(View.Zip(coll, xs))
  }

  case class ArrayView[A](xs: Array[A]) extends IndexedView[A] {
    def length = xs.length
    def apply(n: Int) = xs(n)
    override def className = "ArrayView"
  }

  /* ---------- Views -------------------------------------------------------*/

  /** Concrete collection type: View */
  trait View[+A] extends Iterable[A] with IterableLike[A, View] {
    override def view = this

    /** Avoid copying if source collection is already a view. */
    override def fromIterable[B](c: Iterable[B]): View[B] = c match {
      case c: View[B] => c
      case _ => View.fromIterator(c.iterator)
    }
    override def className = "View"
  }

  /** This object reifies operations on views as case classes */
  object View {
    def fromIterator[A](it: => Iterator[A]): View[A] = new View[A] {
      def iterator = it
    }

    /** The empty view */
    case object Empty extends View[Nothing] {
      def iterator = Iterator.empty
      override def knownSize = 0
    }

    /** A view with given elements */
    case class Elems[A](xs: A*) extends View[A] {
      def iterator = Iterator(xs: _*)
      override def knownSize = xs.length // should be: xs.knownSize, but A*'s are not sequences in this strawman.
    }

    /** A view that filters an underlying collection. */
    case class Filter[A](val underlying: Iterable[A], p: A => Boolean) extends View[A] {
      def iterator = underlying.iterator.filter(p)
    }

    /** A view that partitions an underlying collection into two views */
    case class Partition[A](val underlying: Iterable[A], p: A => Boolean) {

      /** The view consisting of all elements of the underlying collection
       *  that satisfy `p`.
       */
      val left = Partitioned(this, true)

      /** The view consisting of all elements of the underlying collection
       *  that do not satisfy `p`.
       */
      val right = Partitioned(this, false)
    }

    /** A view representing one half of a partition. */
    case class Partitioned[A](partition: Partition[A], cond: Boolean) extends View[A] {
      def iterator = partition.underlying.iterator.filter(x => partition.p(x) == cond)
    }

    /** A view that drops leading elements of the underlying collection. */
    case class Drop[A](underlying: Iterable[A], n: Int) extends View[A] {
      def iterator = underlying.iterator.drop(n)
      protected val normN = n max 0
      override def knownSize =
        if (underlying.knownSize >= 0) (underlying.knownSize - normN) max 0 else -1
    }

    /** A view that takes leading elements of the underlying collection. */
    case class Take[A](underlying: Iterable[A], n: Int) extends View[A] {
      def iterator = underlying.iterator.take(n)
      protected val normN = n max 0
      override def knownSize =
        if (underlying.knownSize >= 0) underlying.knownSize min normN else -1
    }

    /** A view that maps elements of the underlying collection. */
    case class Map[A, B](underlying: Iterable[A], f: A => B) extends View[B] {
      def iterator = underlying.iterator.map(f)
      override def knownSize = underlying.knownSize
    }

    /** A view that flatmaps elements of the underlying collection. */
    case class FlatMap[A, B](underlying: Iterable[A], f: A => IterableOnce[B]) extends View[B] {
      def iterator = underlying.iterator.flatMap(f)
    }

    /** A view that concatenates elements of the underlying collection with the elements
     *  of another collection or iterator.
     */
    case class Concat[A](underlying: Iterable[A], other: IterableOnce[A]) extends View[A] {
      def iterator = underlying.iterator ++ other
      override def knownSize = other match {
        case other: Iterable[_] if underlying.knownSize >= 0 && other.knownSize >= 0 =>
          underlying.knownSize + other.knownSize
        case _ =>
          -1
      }
    }

    /** A view that zips elements of the underlying collection with the elements
     *  of another collection or iterator.
     */
    case class Zip[A, B](underlying: Iterable[A], other: IterableOnce[B]) extends View[(A, B)] {
      def iterator = underlying.iterator.zip(other)
      override def knownSize = other match {
        case other: Iterable[_] if underlying.knownSize >= 0 && other.knownSize >= 0 =>
          underlying.knownSize min other.knownSize
        case _ =>
          -1
      }
    }
  }

  /** View defined in terms of indexing a range */
  trait IndexedView[+A] extends View[A] with ArrayLike[A] { self =>

    def iterator: Iterator[A] = new Iterator[A] {
      private var current = 0
      def hasNext = current < self.length
      def next: A = {
        val r = apply(current)
        current += 1
        r
      }
    }

    override def take(n: Int): IndexedView[A] = new IndexedView.Take(this, n)
    override def drop(n: Int): IndexedView[A] = new IndexedView.Drop(this, n)
    override def map[B](f: A => B): IndexedView[B] = new IndexedView.Map(this, f)
    def reverse: IndexedView[A] = new IndexedView.Reverse(this)
  }

  object IndexedView {

    class Take[A](underlying: IndexedView[A], n: Int)
    extends View.Take(underlying, n) with IndexedView[A] {
      override def iterator = super.iterator // needed to avoid "conflicting overrides" error
      def length = underlying.length min normN
      def apply(i: Int) = underlying.apply(i)
    }

    class Drop[A](underlying: IndexedView[A], n: Int)
    extends View.Take(underlying, n) with IndexedView[A] {
      override def iterator = super.iterator
      def length = (underlying.length - normN) max 0
      def apply(i: Int) = underlying.apply(i + normN)
    }

    class Map[A, B](underlying: IndexedView[A], f: A => B)
    extends View.Map(underlying, f) with IndexedView[B] {
      override def iterator = super.iterator
      def length = underlying.length
      def apply(n: Int) = f(underlying.apply(n))
    }

    case class Reverse[A](underlying: IndexedView[A]) extends IndexedView[A] {
      def length = underlying.length
      def apply(i: Int) = underlying.apply(length - 1 - i)
    }
  }

/* ---------- Iterators ---------------------------------------------------*/

  /** A core Iterator class */
  trait Iterator[+A] extends IterableOnce[A] { self =>
    def hasNext: Boolean
    def next(): A
    def iterator = this
    def foldLeft[B](z: B)(op: (B, A) => B): B =
      if (hasNext) foldLeft(op(z, next))(op) else z
    def foldRight[B](z: B)(op: (A, B) => B): B =
      if (hasNext) op(next(), foldRight(z)(op)) else z
    def foreach(f: A => Unit): Unit =
      while (hasNext) f(next())
    def indexWhere(p: A => Boolean): Int = {
      var i = 0
      while (hasNext) {
        if (p(next())) return i
        i += 1
      }
      -1
    }
    def length = {
      var len = 0
      while (hasNext) { len += 1; next() }
      len
    }
    def filter(p: A => Boolean): Iterator[A] = new Iterator[A] {
      private var hd: A = _
      private var hdDefined: Boolean = false

      def hasNext: Boolean = hdDefined || {
        do {
          if (!self.hasNext) return false
          hd = self.next()
        } while (!p(hd))
        hdDefined = true
        true
      }

      def next() =
        if (hasNext) {
          hdDefined = false
          hd
        }
        else Iterator.empty.next()
    }
    def map[B](f: A => B): Iterator[B] = new Iterator[B] {
      def hasNext = self.hasNext
      def next() = f(self.next())
    }

    def flatMap[B](f: A => IterableOnce[B]): Iterator[B] = new Iterator[B] {
      private var myCurrent: Iterator[B] = Iterator.empty
      private def current = {
        while (!myCurrent.hasNext && self.hasNext)
          myCurrent = f(self.next()).iterator
        myCurrent
      }
      def hasNext = current.hasNext
      def next() = current.next()
    }
    def ++[B >: A](xs: IterableOnce[B]): Iterator[B] = new Iterator[B] {
      private var myCurrent: Iterator[B] = self
      private var first = true
      private def current = {
        if (!myCurrent.hasNext && first) {
          myCurrent = xs.iterator
          first = false
        }
        myCurrent
      }
      def hasNext = current.hasNext
      def next() = current.next()
    }
    def take(n: Int): Iterator[A] = new Iterator[A] {
      private var i = 0
      def hasNext = self.hasNext && i < n
      def next =
        if (hasNext) {
          i += 1
          self.next()
        }
        else Iterator.empty.next()
    }
    def drop(n: Int): Iterator[A] = {
      var i = 0
      while (i < n && hasNext) {
        next()
        i += 1
      }
      this
    }
    def zip[B](that: IterableOnce[B]): Iterator[(A, B)] = new Iterator[(A, B)] {
      val thatIterator = that.iterator
      def hasNext = self.hasNext && thatIterator.hasNext
      def next() = (self.next(), thatIterator.next())
    }
  }

  object Iterator {
    val empty: Iterator[Nothing] = new Iterator[Nothing] {
      def hasNext = false
      def next = throw new NoSuchElementException("next on empty iterator")
    }
    def apply[A](xs: A*): Iterator[A] = new IndexedView[A] {
      val length = xs.length
      def apply(n: Int) = xs(n)
    }.iterator
  }
}
