/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection

import scala.collection.immutable.NumericRange
import scala.language.implicitConversions
import scala.collection.mutable.Builder
import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.ClassTag

/**
  * A factory that builds a collection of type `C` with elements of type `A`.
  *
  * This is a general form of any factory ([[IterableFactory]],
  * [[SortedIterableFactory]], [[MapFactory]] and [[SortedMapFactory]]) whose
  * element type is fixed.
  *
  * @tparam A Type of elements (e.g. `Int`, `Boolean`, etc.)
  * @tparam C Type of collection (e.g. `List[Int]`, `TreeMap[Int, String]`, etc.)
  */
trait Factory[-A, +C] extends Any {

  /**
    * @return A collection of type `C` containing the same elements
    *         as the source collection `it`.
    * @param it Source collection
    */
  def fromSpecific(it: IterableOnce[A]): C

  /** Get a Builder for the collection. For non-strict collection types this will use an intermediate buffer.
    * Building collections with `fromSpecific` is preferred because it can be lazy for lazy collections. */
  def newBuilder: Builder[A, C]
}

object Factory {

  implicit val stringFactory: Factory[Char, String] = new StringFactory
  @SerialVersionUID(3L)
  private class StringFactory extends Factory[Char, String] with Serializable {
    def fromSpecific(it: IterableOnce[Char]): String = {
      val b = new mutable.StringBuilder(scala.math.max(0, it.knownSize))
      b ++= it
      b.result()
    }
    def newBuilder: Builder[Char, String] = new mutable.StringBuilder()
  }

  implicit def arrayFactory[A: ClassTag]: Factory[A, Array[A]] = new ArrayFactory[A]
  @SerialVersionUID(3L)
  private class ArrayFactory[A: ClassTag] extends Factory[A, Array[A]] with Serializable {
    def fromSpecific(it: IterableOnce[A]): Array[A] = {
      val b = newBuilder
      b.sizeHint(it, delta = 0)
      b ++= it
      b.result()
    }
    def newBuilder: Builder[A, Array[A]] = mutable.ArrayBuilder.make[A]
  }

}

/** Base trait for companion objects of unconstrained collection types that may require
  * multiple traversals of a source collection to build a target collection `CC`.
  *
  * @tparam CC Collection type constructor (e.g. `List`)
  * @define factoryInfo
  *   This object provides a set of operations to create $Coll values.
  *
  * @define coll collection
  * @define Coll `Iterable`
  */
trait IterableFactory[+CC[_]] extends Serializable {

  /** Creates a target $coll from an existing source collection
    *
    * @param source Source collection
    * @tparam A the type of the collection’s elements
    * @return a new $coll with the elements of `source`
    */
  def from[A](source: IterableOnce[A]): CC[A]

  /** An empty collection
    * @tparam A      the type of the ${coll}'s elements
    */
  def empty[A]: CC[A]

  /** Creates a $coll with the specified elements.
    * @tparam A     the type of the ${coll}'s elements
    * @param elems  the elements of the created $coll
    * @return a new $coll with elements `elems`
    */
  def apply[A](elems: A*): CC[A] = from(elems)

  /** Produces a $coll containing repeated applications of a function to a start value.
    *
    *  @param start the start value of the $coll
    *  @param len   the number of elements contained in the $coll
    *  @param f     the function that's repeatedly applied
    *  @return      a $coll with `len` values in the sequence `start, f(start), f(f(start)), ...`
    */
  def iterate[A](start: A, len: Int)(f: A => A): CC[A] = from(new View.Iterate(start, len)(f))

  /** Produces a $coll that uses a function `f` to produce elements of type `A`
    * and update an internal state of type `S`.
    *
    * @param init State initial value
    * @param f    Computes the next element (or returns `None` to signal
    *             the end of the collection)
    * @tparam A   Type of the elements
    * @tparam S   Type of the internal state
    * @return a $coll that produces elements using `f` until `f` returns `None`
    */
  def unfold[A, S](init: S)(f: S => Option[(A, S)]): CC[A] = from(new View.Unfold(init)(f))

  /** Produces a $coll containing a sequence of increasing of integers.
    *
    *  @param start the first element of the $coll
    *  @param end   the end value of the $coll (the first value NOT contained)
    *  @return  a $coll with values `start, start + 1, ..., end - 1`
    */
  def range[A : Integral](start: A, end: A): CC[A] = from(NumericRange(start, end, implicitly[Integral[A]].one))

  /** Produces a $coll containing equally spaced values in some integer interval.
    *  @param start the start value of the $coll
    *  @param end   the end value of the $coll (the first value NOT contained)
    *  @param step  the difference between successive elements of the $coll (must be positive or negative)
    *  @return      a $coll with values `start, start + step, ...` up to, but excluding `end`
    */
  def range[A : Integral](start: A, end: A, step: A): CC[A] = from(NumericRange(start, end, step))

  /**
    * @return A builder for $Coll objects.
    * @tparam A the type of the ${coll}’s elements
    */
  def newBuilder[A]: Builder[A, CC[A]]

  /** Produces a $coll containing the results of some element computation a number of times.
    *  @param   n  the number of elements contained in the $coll.
    *  @param   elem the element computation
    *  @return  A $coll that contains the results of `n` evaluations of `elem`.
    */
  def fill[A](n: Int)(elem: => A): CC[A] = from(new View.Fill(n)(elem))

  /** Produces a two-dimensional $coll containing the results of some element computation a number of times.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   elem the element computation
    *  @return  A $coll that contains the results of `n1 x n2` evaluations of `elem`.
    */
  def fill[A](n1: Int, n2: Int)(elem: => A): CC[CC[A] @uncheckedVariance] = fill(n1)(fill(n2)(elem))

  /** Produces a three-dimensional $coll containing the results of some element computation a number of times.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   n3  the number of elements in the 3rd dimension
    *  @param   elem the element computation
    *  @return  A $coll that contains the results of `n1 x n2 x n3` evaluations of `elem`.
    */
  def fill[A](n1: Int, n2: Int, n3: Int)(elem: => A): CC[CC[CC[A]] @uncheckedVariance] = fill(n1)(fill(n2, n3)(elem))

  /** Produces a four-dimensional $coll containing the results of some element computation a number of times.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   n3  the number of elements in the 3rd dimension
    *  @param   n4  the number of elements in the 4th dimension
    *  @param   elem the element computation
    *  @return  A $coll that contains the results of `n1 x n2 x n3 x n4` evaluations of `elem`.
    */
  def fill[A](n1: Int, n2: Int, n3: Int, n4: Int)(elem: => A): CC[CC[CC[CC[A]]] @uncheckedVariance] =
    fill(n1)(fill(n2, n3, n4)(elem))

  /** Produces a five-dimensional $coll containing the results of some element computation a number of times.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   n3  the number of elements in the 3rd dimension
    *  @param   n4  the number of elements in the 4th dimension
    *  @param   n5  the number of elements in the 5th dimension
    *  @param   elem the element computation
    *  @return  A $coll that contains the results of `n1 x n2 x n3 x n4 x n5` evaluations of `elem`.
    */
  def fill[A](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(elem: => A): CC[CC[CC[CC[CC[A]]]] @uncheckedVariance] =
    fill(n1)(fill(n2, n3, n4, n5)(elem))

  /** Produces a $coll containing values of a given function over a range of integer values starting from 0.
    *  @param  n   The number of elements in the $coll
    *  @param  f   The function computing element values
    *  @return A $coll consisting of elements `f(0), ..., f(n -1)`
    */
  def tabulate[A](n: Int)(f: Int => A): CC[A] = from(new View.Tabulate(n)(f))

  /** Produces a two-dimensional $coll containing values of a given function over ranges of integer values starting from 0.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   f   The function computing element values
    *  @return A $coll consisting of elements `f(i1, i2)`
    *          for `0 <= i1 < n1` and `0 <= i2 < n2`.
    */
  def tabulate[A](n1: Int, n2: Int)(f: (Int, Int) => A): CC[CC[A] @uncheckedVariance] =
    tabulate(n1)(i1 => tabulate(n2)(f(i1, _)))

  /** Produces a three-dimensional $coll containing values of a given function over ranges of integer values starting from 0.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   n3  the number of elements in the 3rd dimension
    *  @param   f   The function computing element values
    *  @return A $coll consisting of elements `f(i1, i2, i3)`
    *          for `0 <= i1 < n1`, `0 <= i2 < n2`, and `0 <= i3 < n3`.
    */
  def tabulate[A](n1: Int, n2: Int, n3: Int)(f: (Int, Int, Int) => A): CC[CC[CC[A]] @uncheckedVariance] =
    tabulate(n1)(i1 => tabulate(n2, n3)(f(i1, _, _)))

  /** Produces a four-dimensional $coll containing values of a given function over ranges of integer values starting from 0.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   n3  the number of elements in the 3rd dimension
    *  @param   n4  the number of elements in the 4th dimension
    *  @param   f   The function computing element values
    *  @return A $coll consisting of elements `f(i1, i2, i3, i4)`
    *          for `0 <= i1 < n1`, `0 <= i2 < n2`, `0 <= i3 < n3`, and `0 <= i4 < n4`.
    */
  def tabulate[A](n1: Int, n2: Int, n3: Int, n4: Int)(f: (Int, Int, Int, Int) => A): CC[CC[CC[CC[A]]] @uncheckedVariance] =
    tabulate(n1)(i1 => tabulate(n2, n3, n4)(f(i1, _, _, _)))

  /** Produces a five-dimensional $coll containing values of a given function over ranges of integer values starting from 0.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   n3  the number of elements in the 3rd dimension
    *  @param   n4  the number of elements in the 4th dimension
    *  @param   n5  the number of elements in the 5th dimension
    *  @param   f   The function computing element values
    *  @return A $coll consisting of elements `f(i1, i2, i3, i4, i5)`
    *          for `0 <= i1 < n1`, `0 <= i2 < n2`, `0 <= i3 < n3`, `0 <= i4 < n4`, and `0 <= i5 < n5`.
    */
  def tabulate[A](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(f: (Int, Int, Int, Int, Int) => A): CC[CC[CC[CC[CC[A]]]] @uncheckedVariance] =
    tabulate(n1)(i1 => tabulate(n2, n3, n4, n5)(f(i1, _, _, _, _)))

  /** Concatenates all argument collections into a single $coll.
   *
   *  @param xss the collections that are to be concatenated.
   *  @return the concatenation of all the collections.
   */
  def concat[A](xss: Iterable[A]*): CC[A] = {
    from(xss.foldLeft(View.empty[A])(_ ++ _))
  }

  implicit def iterableFactory[A]: Factory[A, CC[A]] = IterableFactory.toFactory(this)
}

object IterableFactory {

  /**
    * Fixes the element type of `factory` to `A`
    * @param factory The factory to fix the element type
    * @tparam A Type of elements
    * @tparam CC Collection type constructor of the factory (e.g. `Seq`, `List`)
    * @return A [[Factory]] that uses the given `factory` to build a collection of elements
    *         of type `A`
    */
  implicit def toFactory[A, CC[_]](factory: IterableFactory[CC]): Factory[A, CC[A]] = new ToFactory[A, CC](factory)

  @SerialVersionUID(3L)
  private[this] class ToFactory[A, CC[_]](factory: IterableFactory[CC]) extends Factory[A, CC[A]] with Serializable {
    def fromSpecific(it: IterableOnce[A]): CC[A] = factory.from[A](it)
    def newBuilder: Builder[A, CC[A]] = factory.newBuilder[A]
  }

  implicit def toBuildFrom[A, CC[_]](factory: IterableFactory[CC]): BuildFrom[Any, A, CC[A]] =
    new BuildFrom[Any, A, CC[A]] {
      def fromSpecific(from: Any)(it: IterableOnce[A]) = factory.from(it)
      def newBuilder(from: Any) = factory.newBuilder
    }

  @SerialVersionUID(3L)
  class Delegate[CC[_]](delegate: IterableFactory[CC]) extends IterableFactory[CC] {
    override def apply[A](elems: A*): CC[A] = delegate.apply(elems: _*)
    def empty[A]: CC[A] = delegate.empty
    def from[E](it: IterableOnce[E]): CC[E] = delegate.from(it)
    def newBuilder[A]: Builder[A, CC[A]] = delegate.newBuilder[A]
  }
}

/**
  * @tparam CC Collection type constructor (e.g. `List`)
  */
trait SeqFactory[+CC[A] <: SeqOps[A, Seq, Seq[A]]] extends IterableFactory[CC] {
  import SeqFactory.UnapplySeqWrapper
  final def unapplySeq[A](x: CC[A] @uncheckedVariance): UnapplySeqWrapper[A] = new UnapplySeqWrapper(x) // TODO is uncheckedVariance sound here?
}

object SeqFactory {
  @SerialVersionUID(3L)
  class Delegate[CC[A] <: SeqOps[A, Seq, Seq[A]]](delegate: SeqFactory[CC]) extends SeqFactory[CC] {
    override def apply[A](elems: A*): CC[A] = delegate.apply(elems: _*)
    def empty[A]: CC[A] = delegate.empty
    def from[E](it: IterableOnce[E]): CC[E] = delegate.from(it)
    def newBuilder[A]: Builder[A, CC[A]] = delegate.newBuilder[A]
  }

  final class UnapplySeqWrapper[A](private val c: SeqOps[A, Seq, Seq[A]]) extends AnyVal {
    def isEmpty: false = false
    def get: UnapplySeqWrapper[A] = this
    def lengthCompare(len: Int): Int = c.lengthCompare(len)
    def apply(i: Int): A = c(i)
    def drop(n: Int): scala.Seq[A] = c match {
      case seq: scala.Seq[A] => seq.drop(n)
      case _                 => c.view.drop(n).toSeq
    }
    def toSeq: scala.Seq[A] = c.toSeq
  }
}

trait StrictOptimizedSeqFactory[+CC[A] <: SeqOps[A, Seq, Seq[A]]] extends SeqFactory[CC] {

  override def fill[A](n: Int)(elem: => A): CC[A] = {
    val b = newBuilder[A]
    b.sizeHint(n)
    var i = 0
    while (i < n) {
      b += elem
      i += 1
    }
    b.result()
  }

  override def tabulate[A](n: Int)(f: Int => A): CC[A] = {
    val b = newBuilder[A]
    b.sizeHint(n)
    var i = 0
    while (i < n) {
      b += f(i)
      i += 1
    }
    b.result()
  }

  override def concat[A](xss: Iterable[A]*): CC[A] = {
    val b = newBuilder[A]
    val knownSizes = xss.view.map(_.knownSize)
    if (knownSizes forall (_ >= 0)) {
      b.sizeHint(knownSizes.sum)
    }
    for (xs <- xss) b ++= xs
    b.result()
  }

}

/**
  * @tparam A Type of elements (e.g. `Int`, `Boolean`, etc.)
  * @tparam C Type of collection (e.g. `List[Int]`, `TreeMap[Int, String]`, etc.)
  * @define factoryInfo
  *   This object provides a set of operations to create $Coll values.
  *
  * @define coll collection
  * @define Coll `Iterable`
  */
trait SpecificIterableFactory[-A, +C] extends Factory[A, C] {
  def empty: C
  def apply(xs: A*): C = fromSpecific(xs)
  def fill(n: Int)(elem: => A): C = fromSpecific(new View.Fill(n)(elem))
  def newBuilder: Builder[A, C]

  implicit def specificIterableFactory: Factory[A, C] = this
}

/**
  * @define factoryInfo
  *   This object provides a set of operations to create $Coll values.
  *
  * @define coll collection
  * @define Coll `Iterable`
  */
trait MapFactory[+CC[_, _]] extends Serializable {

  /**
   * An empty Map
   */
  def empty[K, V]: CC[K, V]

  /**
   * A collection of type Map generated from given iterable object.
   */
  def from[K, V](it: IterableOnce[(K, V)]): CC[K, V]

  /**
   * A collection of type Map that contains given key/value bindings.
   */
  def apply[K, V](elems: (K, V)*): CC[K, V] = from(elems)

  /**
   * The default builder for Map objects.
   */
  def newBuilder[K, V]: Builder[(K, V), CC[K, V]]

  /**
   * The default Factory instance for maps.
   */
  implicit def mapFactory[K, V]: Factory[(K, V), CC[K, V]] = MapFactory.toFactory(this)
}

object MapFactory {

  /**
    * Fixes the key and value types of `factory` to `K` and `V`, respectively
    * @param factory The factory to fix the key and value types
    * @tparam K Type of keys
    * @tparam V Type of values
    * @tparam CC Collection type constructor of the factory (e.g. `Map`, `HashMap`, etc.)
    * @return A [[Factory]] that uses the given `factory` to build a map with keys of type `K`
    *         and values of type `V`
    */
  implicit def toFactory[K, V, CC[_, _]](factory: MapFactory[CC]): Factory[(K, V), CC[K, V]] = new ToFactory[K, V, CC](factory)

  @SerialVersionUID(3L)
  private[this] class ToFactory[K, V, CC[_, _]](factory: MapFactory[CC]) extends Factory[(K, V), CC[K, V]] with Serializable {
    def fromSpecific(it: IterableOnce[(K, V)]): CC[K, V] = factory.from[K, V](it)
    def newBuilder: Builder[(K, V), CC[K, V]] = factory.newBuilder[K, V]
  }

  implicit def toBuildFrom[K, V, CC[_, _]](factory: MapFactory[CC]): BuildFrom[Any, (K, V), CC[K, V]] =
    new BuildFrom[Any, (K, V), CC[K, V]] {
      def fromSpecific(from: Any)(it: IterableOnce[(K, V)]) = factory.from(it)
      def newBuilder(from: Any) = factory.newBuilder[K, V]
    }

  @SerialVersionUID(3L)
  class Delegate[C[_, _]](delegate: MapFactory[C]) extends MapFactory[C] {
    override def apply[K, V](elems: (K, V)*): C[K, V] = delegate.apply(elems: _*)
    def from[K, V](it: IterableOnce[(K, V)]): C[K, V] = delegate.from(it)
    def empty[K, V]: C[K, V] = delegate.empty
    def newBuilder[K, V]: Builder[(K, V), C[K, V]] = delegate.newBuilder
  }
}

/** Base trait for companion objects of collections that require an implicit evidence.
  * @tparam CC Collection type constructor (e.g. `ArraySeq`)
  * @tparam Ev Unary type constructor for the implicit evidence required for an element type
  *            (typically `Ordering` or `ClassTag`)
  *
  * @define factoryInfo
  *   This object provides a set of operations to create $Coll values.
  *
  * @define coll collection
  * @define Coll `Iterable`
  */
trait EvidenceIterableFactory[+CC[_], Ev[_]] extends Serializable {

  def from[E : Ev](it: IterableOnce[E]): CC[E]

  def empty[A : Ev]: CC[A]

  def apply[A : Ev](xs: A*): CC[A] = from(xs)

  /** Produces a $coll containing the results of some element computation a number of times.
    *  @param   n  the number of elements contained in the $coll.
    *  @param   elem the element computation
    *  @return  A $coll that contains the results of `n` evaluations of `elem`.
    */
  def fill[A : Ev](n: Int)(elem: => A): CC[A] = from(new View.Fill(n)(elem))

  /** Produces a $coll containing values of a given function over a range of integer values starting from 0.
    *  @param  n   The number of elements in the $coll
    *  @param  f   The function computing element values
    *  @return A $coll consisting of elements `f(0), ..., f(n -1)`
    */
  def tabulate[A : Ev](n: Int)(f: Int => A): CC[A] = from(new View.Tabulate(n)(f))

  /** Produces a $coll containing repeated applications of a function to a start value.
    *
    *  @param start the start value of the $coll
    *  @param len   the number of elements contained in the $coll
    *  @param f     the function that's repeatedly applied
    *  @return      a $coll with `len` values in the sequence `start, f(start), f(f(start)), ...`
    */
  def iterate[A : Ev](start: A, len: Int)(f: A => A): CC[A] = from(new View.Iterate(start, len)(f))

  /** Produces a $coll that uses a function `f` to produce elements of type `A`
    * and update an internal state of type `S`.
    *
    * @param init State initial value
    * @param f    Computes the next element (or returns `None` to signal
    *             the end of the collection)
    * @tparam A   Type of the elements
    * @tparam S   Type of the internal state
    * @return a $coll that produces elements using `f` until `f` returns `None`
    */
  def unfold[A : Ev, S](init: S)(f: S => Option[(A, S)]): CC[A] = from(new View.Unfold(init)(f))

  def newBuilder[A : Ev]: Builder[A, CC[A]]

  implicit def evidenceIterableFactory[A : Ev]: Factory[A, CC[A]] = EvidenceIterableFactory.toFactory(this)
}

object EvidenceIterableFactory {

  /**
    * Fixes the element type of `factory` to `A`
    * @param factory The factory to fix the element type
    * @tparam A Type of elements
    * @tparam CC Collection type constructor of the factory (e.g. `TreeSet`)
    * @tparam Ev Type constructor of the evidence (usually `Ordering` or `ClassTag`)
    * @return A [[Factory]] that uses the given `factory` to build a collection of elements
    *         of type `A`
    */
  implicit def toFactory[Ev[_], A: Ev, CC[_]](factory: EvidenceIterableFactory[CC, Ev]): Factory[A, CC[A]] = new ToFactory[Ev, A, CC](factory)

  @SerialVersionUID(3L)
  private[this] class ToFactory[Ev[_], A: Ev, CC[_]](factory: EvidenceIterableFactory[CC, Ev]) extends Factory[A, CC[A]] with Serializable {
    def fromSpecific(it: IterableOnce[A]): CC[A] = factory.from[A](it)
    def newBuilder: Builder[A, CC[A]] = factory.newBuilder[A]
  }

  implicit def toBuildFrom[Ev[_], A: Ev, CC[_]](factory: EvidenceIterableFactory[CC, Ev]): BuildFrom[Any, A, CC[A]] = new EvidenceIterableFactoryToBuildFrom(factory)
  private class EvidenceIterableFactoryToBuildFrom[Ev[_], A: Ev, CC[_]](factory: EvidenceIterableFactory[CC, Ev]) extends BuildFrom[Any, A, CC[A]] {
    def fromSpecific(from: Any)(it: IterableOnce[A]): CC[A] = factory.from[A](it)
    def newBuilder(from: Any): Builder[A, CC[A]] = factory.newBuilder[A]
  }

  @SerialVersionUID(3L)
  class Delegate[CC[_], Ev[_]](delegate: EvidenceIterableFactory[CC, Ev]) extends EvidenceIterableFactory[CC, Ev] {
    override def apply[A: Ev](xs: A*): CC[A] = delegate.apply(xs: _*)
    def empty[A : Ev]: CC[A] = delegate.empty
    def from[E : Ev](it: IterableOnce[E]): CC[E] = delegate.from(it)
    def newBuilder[A : Ev]: Builder[A, CC[A]] = delegate.newBuilder[A]
  }
}

/** Base trait for companion objects of collections that require an implicit `Ordering`.
  * @tparam CC Collection type constructor (e.g. `SortedSet`)
  */
trait SortedIterableFactory[+CC[_]] extends EvidenceIterableFactory[CC, Ordering]

object SortedIterableFactory {
  @SerialVersionUID(3L)
  class Delegate[CC[_]](delegate: EvidenceIterableFactory[CC, Ordering])
    extends EvidenceIterableFactory.Delegate[CC, Ordering](delegate) with SortedIterableFactory[CC]
}

/** Base trait for companion objects of collections that require an implicit `ClassTag`.
  * @tparam CC Collection type constructor (e.g. `ArraySeq`)
  */
trait ClassTagIterableFactory[+CC[_]] extends EvidenceIterableFactory[CC, ClassTag] {

  @`inline` private[this] implicit def ccClassTag[X]: ClassTag[CC[X]] =
    ClassTag.AnyRef.asInstanceOf[ClassTag[CC[X]]] // Good enough for boxed vs primitive arrays

  /** Produces a $coll containing a sequence of increasing of integers.
    *
    *  @param start the first element of the $coll
    *  @param end   the end value of the $coll (the first value NOT contained)
    *  @return  a $coll with values `start, start + 1, ..., end - 1`
    */
  def range[A : Integral : ClassTag](start: A, end: A): CC[A] = from(NumericRange(start, end, implicitly[Integral[A]].one))

  /** Produces a $coll containing equally spaced values in some integer interval.
    *  @param start the start value of the $coll
    *  @param end   the end value of the $coll (the first value NOT contained)
    *  @param step  the difference between successive elements of the $coll (must be positive or negative)
    *  @return      a $coll with values `start, start + step, ...` up to, but excluding `end`
    */
  def range[A : Integral : ClassTag](start: A, end: A, step: A): CC[A] = from(NumericRange(start, end, step))

  /** Produces a two-dimensional $coll containing the results of some element computation a number of times.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   elem the element computation
    *  @return  A $coll that contains the results of `n1 x n2` evaluations of `elem`.
    */
  def fill[A : ClassTag](n1: Int, n2: Int)(elem: => A): CC[CC[A] @uncheckedVariance] = fill(n1)(fill(n2)(elem))

  /** Produces a three-dimensional $coll containing the results of some element computation a number of times.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   n3  the number of elements in the 3rd dimension
    *  @param   elem the element computation
    *  @return  A $coll that contains the results of `n1 x n2 x n3` evaluations of `elem`.
    */
  def fill[A : ClassTag](n1: Int, n2: Int, n3: Int)(elem: => A): CC[CC[CC[A]] @uncheckedVariance] = fill(n1)(fill(n2, n3)(elem))

  /** Produces a four-dimensional $coll containing the results of some element computation a number of times.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   n3  the number of elements in the 3rd dimension
    *  @param   n4  the number of elements in the 4th dimension
    *  @param   elem the element computation
    *  @return  A $coll that contains the results of `n1 x n2 x n3 x n4` evaluations of `elem`.
    */
  def fill[A : ClassTag](n1: Int, n2: Int, n3: Int, n4: Int)(elem: => A): CC[CC[CC[CC[A]]] @uncheckedVariance] =
    fill(n1)(fill(n2, n3, n4)(elem))

  /** Produces a five-dimensional $coll containing the results of some element computation a number of times.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   n3  the number of elements in the 3rd dimension
    *  @param   n4  the number of elements in the 4th dimension
    *  @param   n5  the number of elements in the 5th dimension
    *  @param   elem the element computation
    *  @return  A $coll that contains the results of `n1 x n2 x n3 x n4 x n5` evaluations of `elem`.
    */
  def fill[A : ClassTag](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(elem: => A): CC[CC[CC[CC[CC[A]]]] @uncheckedVariance] =
    fill(n1)(fill(n2, n3, n4, n5)(elem))

  /** Produces a two-dimensional $coll containing values of a given function over ranges of integer values starting from 0.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   f   The function computing element values
    *  @return A $coll consisting of elements `f(i1, i2)`
    *          for `0 <= i1 < n1` and `0 <= i2 < n2`.
    */
  def tabulate[A : ClassTag](n1: Int, n2: Int)(f: (Int, Int) => A): CC[CC[A] @uncheckedVariance] =
    tabulate(n1)(i1 => tabulate(n2)(f(i1, _)))

  /** Produces a three-dimensional $coll containing values of a given function over ranges of integer values starting from 0.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   n3  the number of elements in the 3rd dimension
    *  @param   f   The function computing element values
    *  @return A $coll consisting of elements `f(i1, i2, i3)`
    *          for `0 <= i1 < n1`, `0 <= i2 < n2`, and `0 <= i3 < n3`.
    */
  def tabulate[A : ClassTag](n1: Int, n2: Int, n3: Int)(f: (Int, Int, Int) => A): CC[CC[CC[A]] @uncheckedVariance] =
    tabulate(n1)(i1 => tabulate(n2, n3)(f(i1, _, _)))

  /** Produces a four-dimensional $coll containing values of a given function over ranges of integer values starting from 0.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   n3  the number of elements in the 3rd dimension
    *  @param   n4  the number of elements in the 4th dimension
    *  @param   f   The function computing element values
    *  @return A $coll consisting of elements `f(i1, i2, i3, i4)`
    *          for `0 <= i1 < n1`, `0 <= i2 < n2`, `0 <= i3 < n3`, and `0 <= i4 < n4`.
    */
  def tabulate[A : ClassTag](n1: Int, n2: Int, n3: Int, n4: Int)(f: (Int, Int, Int, Int) => A): CC[CC[CC[CC[A]]] @uncheckedVariance] =
    tabulate(n1)(i1 => tabulate(n2, n3, n4)(f(i1, _, _, _)))

  /** Produces a five-dimensional $coll containing values of a given function over ranges of integer values starting from 0.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   n3  the number of elements in the 3rd dimension
    *  @param   n4  the number of elements in the 4th dimension
    *  @param   n5  the number of elements in the 5th dimension
    *  @param   f   The function computing element values
    *  @return A $coll consisting of elements `f(i1, i2, i3, i4, i5)`
    *          for `0 <= i1 < n1`, `0 <= i2 < n2`, `0 <= i3 < n3`, `0 <= i4 < n4`, and `0 <= i5 < n5`.
    */
  def tabulate[A : ClassTag](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(f: (Int, Int, Int, Int, Int) => A): CC[CC[CC[CC[CC[A]]]] @uncheckedVariance] =
    tabulate(n1)(i1 => tabulate(n2, n3, n4, n5)(f(i1, _, _, _, _)))
}

object ClassTagIterableFactory {
  @SerialVersionUID(3L)
  class Delegate[CC[_]](delegate: EvidenceIterableFactory[CC, ClassTag])
    extends EvidenceIterableFactory.Delegate[CC, ClassTag](delegate) with ClassTagIterableFactory[CC]

  /** An IterableFactory that uses ClassTag.Any as the evidence for every element type. This may or may not be
    * sound depending on the use of the `ClassTag` by the collection implementation. */
  @SerialVersionUID(3L)
  class AnyIterableDelegate[CC[_]](delegate: ClassTagIterableFactory[CC]) extends IterableFactory[CC] {
    def empty[A]: CC[A] = delegate.empty(using ClassTag.Any).asInstanceOf[CC[A]]
    def from[A](it: IterableOnce[A]): CC[A] = delegate.from[Any](it)(using ClassTag.Any).asInstanceOf[CC[A]]
    def newBuilder[A]: Builder[A, CC[A]] = delegate.newBuilder(using ClassTag.Any).asInstanceOf[Builder[A, CC[A]]]
    override def apply[A](elems: A*): CC[A] = delegate.apply[Any](elems: _*)(using ClassTag.Any).asInstanceOf[CC[A]]
    override def iterate[A](start: A, len: Int)(f: A => A): CC[A] = delegate.iterate[A](start, len)(f)(using ClassTag.Any.asInstanceOf[ClassTag[A]])
    override def unfold[A, S](init: S)(f: S => Option[(A, S)]): CC[A] = delegate.unfold[A, S](init)(f)(using ClassTag.Any.asInstanceOf[ClassTag[A]])
    override def range[A](start: A, end: A)(implicit i: Integral[A]): CC[A] = delegate.range[A](start, end)(using i, ClassTag.Any.asInstanceOf[ClassTag[A]])
    override def range[A](start: A, end: A, step: A)(implicit i: Integral[A]): CC[A] = delegate.range[A](start, end, step)(using i, ClassTag.Any.asInstanceOf[ClassTag[A]])
    override def fill[A](n: Int)(elem: => A): CC[A] = delegate.fill[Any](n)(elem)(using ClassTag.Any).asInstanceOf[CC[A]]
    override def tabulate[A](n: Int)(f: Int => A): CC[A] = delegate.tabulate[Any](n)(f)(using ClassTag.Any).asInstanceOf[CC[A]]
  }
}

/**
  * @tparam CC Collection type constructor (e.g. `ArraySeq`)
  */
trait ClassTagSeqFactory[+CC[A] <: SeqOps[A, Seq, Seq[A]]] extends ClassTagIterableFactory[CC] {
  import SeqFactory.UnapplySeqWrapper
  final def unapplySeq[A](x: CC[A] @uncheckedVariance): UnapplySeqWrapper[A] = new UnapplySeqWrapper(x) // TODO is uncheckedVariance sound here?
}

object ClassTagSeqFactory {
  @SerialVersionUID(3L)
  class Delegate[CC[A] <: SeqOps[A, Seq, Seq[A]]](delegate: ClassTagSeqFactory[CC])
    extends ClassTagIterableFactory.Delegate[CC](delegate) with ClassTagSeqFactory[CC]

  /** A SeqFactory that uses ClassTag.Any as the evidence for every element type. This may or may not be
    * sound depending on the use of the `ClassTag` by the collection implementation. */
  @SerialVersionUID(3L)
  class AnySeqDelegate[CC[A] <: SeqOps[A, Seq, Seq[A]]](delegate: ClassTagSeqFactory[CC])
    extends ClassTagIterableFactory.AnyIterableDelegate[CC](delegate) with SeqFactory[CC]
}

trait StrictOptimizedClassTagSeqFactory[+CC[A] <: SeqOps[A, Seq, Seq[A]]] extends ClassTagSeqFactory[CC] {

  override def fill[A : ClassTag](n: Int)(elem: => A): CC[A] = {
    val b = newBuilder[A]
    b.sizeHint(n)
    var i = 0
    while (i < n) {
      b += elem
      i += 1
    }
    b.result()
  }

  override def tabulate[A : ClassTag](n: Int)(f: Int => A): CC[A] = {
    val b = newBuilder[A]
    b.sizeHint(n)
    var i = 0
    while (i < n) {
      b += f(i)
      i += 1
    }
    b.result()
  }

}

/**
  * @define factoryInfo
  *   This object provides a set of operations to create $Coll values.
  *
  * @define coll collection
  * @define Coll `Iterable`
  */
trait SortedMapFactory[+CC[_, _]] extends Serializable {

  def empty[K : Ordering, V]: CC[K, V]

  def from[K : Ordering, V](it: IterableOnce[(K, V)]): CC[K, V]

  def apply[K : Ordering, V](elems: (K, V)*): CC[K, V] = from(elems)

  def newBuilder[K : Ordering, V]: Builder[(K, V), CC[K, V]]

  implicit def sortedMapFactory[K : Ordering, V]: Factory[(K, V), CC[K, V]] = SortedMapFactory.toFactory(this)

}

object SortedMapFactory {

  /**
    * Implicit conversion that fixes the key and value types of `factory` to `K` and `V`,
    * respectively.
    *
    * @param factory The factory to fix the key and value types
    * @tparam K Type of keys
    * @tparam V Type of values
    * @tparam CC Collection type constructor of the factory (e.g. `TreeMap`)
    * @return A [[Factory]] that uses the given `factory` to build a map with keys of
    *         type `K` and values of type `V`
    */
  implicit def toFactory[K : Ordering, V, CC[_, _]](factory: SortedMapFactory[CC]): Factory[(K, V), CC[K, V]] = new ToFactory[K, V, CC](factory)

  @SerialVersionUID(3L)
  private[this] class ToFactory[K : Ordering, V, CC[_, _]](factory: SortedMapFactory[CC]) extends Factory[(K, V), CC[K, V]] with Serializable {
    def fromSpecific(it: IterableOnce[(K, V)]): CC[K, V] = factory.from[K, V](it)
    def newBuilder: Builder[(K, V), CC[K, V]] = factory.newBuilder[K, V]
  }

  implicit def toBuildFrom[K : Ordering, V, CC[_, _]](factory: SortedMapFactory[CC]): BuildFrom[Any, (K, V), CC[K, V]] = new SortedMapFactoryToBuildFrom(factory)
  private class SortedMapFactoryToBuildFrom[K : Ordering, V, CC[_, _]](factory: SortedMapFactory[CC]) extends BuildFrom[Any, (K, V), CC[K, V]] {
    def fromSpecific(from: Any)(it: IterableOnce[(K, V)]) = factory.from(it)
    def newBuilder(from: Any) = factory.newBuilder[K, V]
  }

  @SerialVersionUID(3L)
  class Delegate[CC[_, _]](delegate: SortedMapFactory[CC]) extends SortedMapFactory[CC] {
    override def apply[K: Ordering, V](elems: (K, V)*): CC[K, V] = delegate.apply(elems: _*)
    def from[K : Ordering, V](it: IterableOnce[(K, V)]): CC[K, V] = delegate.from(it)
    def empty[K : Ordering, V]: CC[K, V] = delegate.empty
    def newBuilder[K : Ordering, V]: Builder[(K, V), CC[K, V]] = delegate.newBuilder
  }
}
