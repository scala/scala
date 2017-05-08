package strawman

import scala.{Any, AnyVal, Array, Char, Int, Unit}
import scala.Predef.String
import scala.reflect.ClassTag

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
 *         iterator, fromIterable, fromSpecificIterable, knownLength, className
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
package object collection extends LowPriority {
  import scala.language.implicitConversions
  // ------------------ Decorators to add collection ops to existing types -----------------------

  /** Decorator to add collection operations to strings. */
  implicit def stringToStringOps(s: String): StringOps = new StringOps(s)

  /** Decorator to add collection operations to arrays. */
  implicit def arrayToArrayOps[A](as: Array[A]): ArrayOps[A] = new ArrayOps[A](as)

  implicit class toNewIterator[A](val it: scala.Iterator[A]) extends AnyVal {
    def toStrawman = new strawman.collection.Iterator[A] {
      def hasNext = it.hasNext
      def next() = it.next()
    }
  }

  implicit class toOldIterator[A](val it: strawman.collection.Iterator[A]) extends AnyVal {
    def toClassic = new scala.Iterator[A] {
      def hasNext = it.hasNext
      def next() = it.next()
    }
  }

  implicit class toNewSeq[A](val s: scala.collection.Seq[A]) extends AnyVal {
    def toStrawman: strawman.collection.Seq[A] =
      new strawman.collection.mutable.ArrayBuffer() ++= s.iterator.toStrawman
  }

  implicit class toOldSeq[A](val s: strawman.collection.Seq[A]) extends AnyVal {
    def toClassic: scala.collection.Seq[A] =
      new scala.collection.mutable.ArrayBuffer ++= s.iterator().toClassic
  }

  /** Needed to circumvent a difficulty between dotty and scalac concerning
   *  the right top type for a type parameter of kind * -> *.
   *  In Scalac, we can provide `Any`, as `Any` is kind-polymorphic. In dotty this is not allowed.
   *  In dotty, we can provide `[X] => Any`. But Scalac does not know lambda syntax.
   */
  type AnyConstr[X] = Any
}

class LowPriority {
  import scala.language.implicitConversions
  import strawman.collection._

  /** Convert array to iterable via view. Lower priority than ArrayOps */
  implicit def arrayToView[T](xs: Array[T]): ArrayView[T] = new ArrayView[T](xs)

  /** Convert string to iterable via view. Lower priority than StringOps */
  implicit def stringToView(s: String): StringView = new StringView(s)
}
