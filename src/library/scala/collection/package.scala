package scala


package object collection extends LowPriority {
  @deprecated("Use Iterable instead of Traversable", "2.13.0")
  type Traversable[+X] = Iterable[X]
  @deprecated("Use Iterable instead of Traversable", "2.13.0")
  val Traversable = Iterable
  @deprecated("Use SeqOps instead of SeqLike", "2.13.0")
  type SeqLike[A, T] = SeqOps[A, Seq, T]
  @deprecated("Use SeqOps (for the methods) or IndexedSeqOps (for fast indexed access) instead of ArrayLike", "2.13.0")
  type ArrayLike[A] = SeqOps[A, Seq, Seq[A]]

  @deprecated("Gen* collection types have been removed", "2.13.0")
  type GenTraversableOnce[+X] = IterableOnce[X]
  @deprecated("Gen* collection types have been removed", "2.13.0")
  val GenTraversableOnce = IterableOnce
  @deprecated("Gen* collection types have been removed", "2.13.0")
  type GenTraversable[+X] = Iterable[X]
  @deprecated("Gen* collection types have been removed", "2.13.0")
  val GenTraversable = Iterable
  @deprecated("Gen* collection types have been removed", "2.13.0")
  type GenIterable[+X] = Iterable[X]
  @deprecated("Gen* collection types have been removed", "2.13.0")
  val GenIterable = Iterable
  @deprecated("Gen* collection types have been removed", "2.13.0")
  type GenSeq[+X] = Seq[X]
  @deprecated("Gen* collection types have been removed", "2.13.0")
  val GenSeq = Seq
  @deprecated("Gen* collection types have been removed", "2.13.0")
  type GenSet[X] = Set[X]
  @deprecated("Gen* collection types have been removed", "2.13.0")
  val GenSet = Set
  @deprecated("Gen* collection types have been removed", "2.13.0")
  type GenMap[K, +V] = Map[K, V]
  @deprecated("Gen* collection types have been removed", "2.13.0")
  val GenMap = Map

  import scala.language.implicitConversions
  // ------------------ Decorators to add collection ops to existing types -----------------------

  /** Decorator to add collection operations to strings. */
  def stringToStringOps(s: String): StringOps = new StringOps(s)

  /** Decorator to add collection operations to arrays. */
  def arrayToArrayOps[A](as: Array[A]): ArrayOps[A] = new ArrayOps[A](as)

  class toNewIterator[A](val it: scala.Iterator[A]) extends AnyVal {
    def toStrawman = new scala.collection.Iterator[A] {
      def hasNext = it.hasNext
      def next() = it.next()
    }
  }

  class toOldIterator[A](val it: scala.collection.Iterator[A]) extends AnyVal {
    def toClassic = new scala.Iterator[A] {
      def hasNext = it.hasNext
      def next() = it.next()
    }
  }

  class toNewSeq[A](val s: scala.collection.Seq[A]) extends AnyVal {
    def toStrawman: scala.collection.Seq[A] =
      new scala.collection.mutable.ArrayBuffer() ++= s.iterator
  }

  class toOldSeq[A](val s: scala.collection.Seq[A]) extends AnyVal {
    def toClassic: scala.collection.Seq[A] =
      new scala.collection.mutable.ArrayBuffer ++= s.iterator()
  }

  /** Needed to circumvent a difficulty between dotty and scalac concerning
   *  the right top type for a type parameter of kind * -> *.
   *  In Scalac, we can provide `Any`, as `Any` is kind-polymorphic. In dotty this is not allowed.
   *  In dotty, we can provide `[X] => Any`. But Scalac does not know lambda syntax.
   */
  type AnyConstr[X] = Any

  /** Collection internal utility functions.
    */
  private[collection] object DebugUtils {
    def unsupported(msg: String)     = throw new UnsupportedOperationException(msg)
    def noSuchElement(msg: String)   = throw new NoSuchElementException(msg)
    def indexOutOfBounds(index: Int) = throw new IndexOutOfBoundsException(index.toString)
    def illegalArgument(msg: String) = throw new IllegalArgumentException(msg)

    def buildString(closure: (Any => Unit) => Unit): String = {
      val output = new collection.mutable.StringBuilder
      closure { any =>
        output ++= any.toString
        output += '\n'
      }

      output.result()
    }

    def arrayString[T](array: Array[T], from: Int, until: Int): String = {
      array.slice(from, until) map ({
        case null => "n/a"
        case x    => "" + x
      }: scala.PartialFunction[T, String]) mkString " | "
    }
  }

  def optionToIterableOnce[A](maybeA: scala.Option[A]): IterableOnce[A] =
     new Iterator[A] {
       private var _hasNext = maybeA.nonEmpty
       def next(): A = if (_hasNext) { _hasNext = false; maybeA.get } else Iterator.empty.next()
       def hasNext: Boolean = _hasNext
     }

  /** An extractor used to head/tail deconstruct sequences. */
  object +: {
    /** Splits a sequence into head :+ tail.
      * @return Some((head, tail)) if sequence is non-empty. None otherwise.
      */
    def unapply[A, CC[_] <: Seq[_], C <: SeqOps[A, CC, C]](t: C with SeqOps[A, CC, C]): Option[(A, C)] =
      if(t.isEmpty) None
      else Some(t.head -> t.tail)
  }

  /** An extractor used to init/last deconstruct sequences. */
  object :+ {
    /** Splits a sequence into init :+ last.
      * @return Some((init, last)) if sequence is non-empty. None otherwise.
      */
    def unapply[A, CC[_] <: Seq[_], C <: SeqOps[A, CC, C]](t: C with SeqOps[A, CC, C]): Option[(C, A)] =
      if(t.isEmpty) None
      else Some(t.init -> t.last)
  }
}

class LowPriority {
  import scala.language.implicitConversions
  import scala.collection._

  /** Convert array to WrappedArray. Lower priority than ArrayOps */
  def arrayToWrappedArray[T](xs: Array[T]): mutable.IndexedSeq[T] = mutable.WrappedArray.make(xs)

  /** Convert String to Seq. Lower priority than StringOps */
  def stringToSeq(s: String): immutable.WrappedString = new immutable.WrappedString(s)
}
