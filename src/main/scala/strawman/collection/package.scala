package strawman

import Predef.{augmentString => _, wrapString => _, _}
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
package object collection extends LowPriority {
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
    def iterator() = coll.iterator()

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
      for (ch <- xs.iterator()) sb += ch
      sb.result
    }

    /** Another overloaded version of `++`. */
    def ++(xs: String): String = s + xs
  }

  /** Decorator to add collection operations to arrays.
   */
  implicit class ArrayOps[A](val xs: Array[A])
  extends AnyVal with IterableOps[A]
     with SeqMonoTransforms[A, Array[A]]
     with Buildable[A, Array[A]]
     with ArrayLike[A] {

    protected def coll = new ArrayView(xs)
    def iterator() = coll.iterator()

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
}

class LowPriority {
  import strawman.collection._

  /** Convert array to iterable via view. Lower priority than ArrayOps */
  implicit def arrayToView[T](xs: Array[T]): ArrayView[T] = new ArrayView[T](xs)

  /** Convert string to iterable via view. Lower priority than StringOps */
  implicit def stringToView(s: String): StringView = new StringView(s)
}
