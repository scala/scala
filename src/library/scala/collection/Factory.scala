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
  def newBuilder(): Builder[A, C]
}

object Factory {

  implicit val stringFactory: Factory[Char, String] =
    new Factory[Char, String] {
      def fromSpecific(it: IterableOnce[Char]): String = {
        val b = new mutable.StringBuilder(scala.math.max(0, it.knownSize))
        b ++= it
        b.result()
      }
      def newBuilder(): Builder[Char, String] = new mutable.StringBuilder()
    }

  implicit def arrayFactory[A: ClassTag]: Factory[A, Array[A]] =
    new Factory[A, Array[A]] {
      def fromSpecific(it: IterableOnce[A]): Array[A] = {
        val b = newBuilder()
        b.sizeHint(scala.math.max(0, it.knownSize))
        b ++= it
        b.result()
      }
      def newBuilder(): Builder[A, Array[A]] = mutable.ArrayBuilder.make[A]()
    }

}

/** Base trait for companion objects of unconstrained collection types that may require
  * multiple traversals of a source collection to build a target collection `CC`.
  *
  * @tparam CC Collection type constructor (e.g. `List`)
  * @define factoryInfo
  *   This object provides a set of operations to create $Coll values.
  *   @author Martin Odersky
  *   @version 2.8
  *
  * @define coll collection
  * @define Coll `Iterable`
  */
trait IterableFactory[+CC[_]] {

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
  def apply[A](elems: A*): CC[A] = from(new View.Elems(elems: _*))

  /** Produces a $coll containing repeated applications of a function to a start value.
    *
    *  @param start the start value of the $coll
    *  @param len   the number of elements contained in the $coll
    *  @param f     the function that's repeatedly applied
    *  @return      a $coll with `len` values in the sequence `start, f(start), f(f(start)), ...`
    */
  def iterate[A](start: A, len: Int)(f: A => A): CC[A] = from(new View.Iterate(start, len)(f))

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
  def newBuilder[A](): Builder[A, CC[A]]

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
    *  @param   n3  the number of elements in the 3nd dimension
    *  @param   elem the element computation
    *  @return  A $coll that contains the results of `n1 x n2 x n3` evaluations of `elem`.
    */
  def fill[A](n1: Int, n2: Int, n3: Int)(elem: => A): CC[CC[CC[A]] @uncheckedVariance] = fill(n1)(fill(n2, n3)(elem))

  /** Produces a four-dimensional $coll containing the results of some element computation a number of times.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   n3  the number of elements in the 3nd dimension
    *  @param   n4  the number of elements in the 4th dimension
    *  @param   elem the element computation
    *  @return  A $coll that contains the results of `n1 x n2 x n3 x n4` evaluations of `elem`.
    */
  def fill[A](n1: Int, n2: Int, n3: Int, n4: Int)(elem: => A): CC[CC[CC[CC[A]]] @uncheckedVariance] =
    fill(n1)(fill(n2, n3, n4)(elem))

  /** Produces a five-dimensional $coll containing the results of some element computation a number of times.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   n3  the number of elements in the 3nd dimension
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
    *  @param   n3  the number of elements in the 3nd dimension
    *  @param   f   The function computing element values
    *  @return A $coll consisting of elements `f(i1, i2, i3)`
    *          for `0 <= i1 < n1`, `0 <= i2 < n2`, and `0 <= i3 < n3`.
    */
  def tabulate[A](n1: Int, n2: Int, n3: Int)(f: (Int, Int, Int) => A): CC[CC[CC[A]] @uncheckedVariance] =
    tabulate(n1)(i1 => tabulate(n2, n3)(f(i1, _, _)))

  /** Produces a four-dimensional $coll containing values of a given function over ranges of integer values starting from 0.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   n3  the number of elements in the 3nd dimension
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
    *  @param   n3  the number of elements in the 3nd dimension
    *  @param   n4  the number of elements in the 4th dimension
    *  @param   n5  the number of elements in the 5th dimension
    *  @param   f   The function computing element values
    *  @return A $coll consisting of elements `f(i1, i2, i3, i4, i5)`
    *          for `0 <= i1 < n1`, `0 <= i2 < n2`, `0 <= i3 < n3`, `0 <= i4 < n4`, and `0 <= i5 < n5`.
    */
  def tabulate[A](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(f: (Int, Int, Int, Int, Int) => A): CC[CC[CC[CC[CC[A]]]] @uncheckedVariance] =
    tabulate(n1)(i1 => tabulate(n2, n3, n4, n5)(f(i1, _, _, _, _)))

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
  implicit def toFactory[A, CC[_]](factory: IterableFactory[CC]): Factory[A, CC[A]] =
    new Factory[A, CC[A]] {
      def fromSpecific(it: IterableOnce[A]): CC[A] = factory.from[A](it)
      def newBuilder(): Builder[A, CC[A]] = factory.newBuilder[A]()
    }

  implicit def toBuildFrom[A, CC[_]](factory: IterableFactory[CC]): BuildFrom[Any, A, CC[A]] =
    new BuildFrom[Any, A, CC[A]] {
      def fromSpecificIterable(from: Any)(it: Iterable[A]) = factory.from(it)
      def newBuilder(from: Any) = factory.newBuilder()
    }

  class Delegate[CC[_]](delegate: IterableFactory[CC]) extends IterableFactory[CC] {
    def empty[A]: CC[A] = delegate.empty
    def from[E](it: IterableOnce[E]): CC[E] = delegate.from(it)
    def newBuilder[A](): Builder[A, CC[A]] = delegate.newBuilder[A]()
  }
}

/**
  * @tparam CC Collection type constructor (e.g. `List`)
  */
trait SeqFactory[+CC[_]] extends IterableFactory[CC] {
  def unapplySeq[A](x: CC[A] @uncheckedVariance): Some[CC[A]] = Some(x) //TODO is uncheckedVariance sound here?
}

object SeqFactory {
  class Delegate[CC[_]](delegate: SeqFactory[CC]) extends SeqFactory[CC] {
    def empty[A]: CC[A] = delegate.empty
    def from[E](it: IterableOnce[E]): CC[E] = delegate.from(it)
    def newBuilder[A](): Builder[A, CC[A]] = delegate.newBuilder[A]()
  }
}

trait StrictOptimizedSeqFactory[+CC[_]] extends SeqFactory[CC] {

  override def fill[A](n: Int)(elem: => A): CC[A] = {
    val b = newBuilder[A]()
    b.sizeHint(n)
    var i = 0
    while (i < n) {
      b += elem
      i += 1
    }
    b.result()
  }

  override def tabulate[A](n: Int)(f: Int => A): CC[A] = {
    val b = newBuilder[A]()
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
  * @tparam A Type of elements (e.g. `Int`, `Boolean`, etc.)
  * @tparam C Type of collection (e.g. `List[Int]`, `TreeMap[Int, String]`, etc.)
  * @define factoryInfo
  *   This object provides a set of operations to create $Coll values.
  *   @author Martin Odersky
  *   @version 2.8
  *
  * @define coll collection
  * @define Coll `Iterable`
  */
trait SpecificIterableFactory[-A, +C] extends Factory[A, C] {
  def empty: C
  def apply(xs: A*): C = fromSpecific(new View.Elems(xs: _*))
  def fill(n: Int)(elem: => A): C = fromSpecific(new View.Fill(n)(elem))
  def newBuilder(): Builder[A, C]

  implicit def specificIterableFactory: Factory[A, C] = this
}

/**
  * @define factoryInfo
  *   This object provides a set of operations to create $Coll values.
  *   @author Martin Odersky
  *   @version 2.8
  *
  * @define coll collection
  * @define Coll `Iterable`
  */
trait MapFactory[+CC[_, _]] {

  def empty[K, V]: CC[K, V]

  def from[K, V](it: IterableOnce[(K, V)]): CC[K, V]

  def apply[K, V](elems: (K, V)*): CC[K, V] = from(elems)

  def newBuilder[K, V](): Builder[(K, V), CC[K, V]]

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
  implicit def toFactory[K, V, CC[_, _]](factory: MapFactory[CC]): Factory[(K, V), CC[K, V]] =
    new Factory[(K, V), CC[K, V]] {
      def fromSpecific(it: IterableOnce[(K, V)]): CC[K, V] = factory.from[K, V](it)
      def newBuilder(): Builder[(K, V), CC[K, V]] = factory.newBuilder[K, V]()
    }

  implicit def toBuildFrom[K, V, CC[_, _]](factory: MapFactory[CC]): BuildFrom[Any, (K, V), CC[K, V]] =
    new BuildFrom[Any, (K, V), CC[K, V]] {
      def fromSpecificIterable(from: Any)(it: Iterable[(K, V)]) = factory.from(it)
      def newBuilder(from: Any) = factory.newBuilder[K, V]()
    }

  class Delegate[C[_, _]](delegate: MapFactory[C]) extends MapFactory[C] {
    def from[K, V](it: IterableOnce[(K, V)]): C[K, V] = delegate.from(it)
    def empty[K, V]: C[K, V] = delegate.empty
    def newBuilder[K, V](): Builder[(K, V), C[K, V]] = delegate.newBuilder()
  }
}

/** Base trait for companion objects of collections that require an implicit evidence.
  * @tparam CC Collection type constructor (e.g. `ImmutableArray`)
  * @tparam Ev Unary type constructor for the implicit evidence required for an element type
  *            (typically `Ordering` or `ClassTag`)
  *
  * @define factoryInfo
  *   This object provides a set of operations to create $Coll values.
  *
  * @define coll collection
  * @define Coll `Iterable`
  */
trait EvidenceIterableFactory[+CC[_], Ev[_]] {

  def from[E : Ev](it: IterableOnce[E]): CC[E]

  def empty[A : Ev]: CC[A]

  def apply[A : Ev](xs: A*): CC[A] = from(new View.Elems(xs: _*))

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

  def newBuilder[A : Ev](): Builder[A, CC[A]]

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
  implicit def toFactory[Ev[_], A: Ev, CC[_]](factory: EvidenceIterableFactory[CC, Ev]): Factory[A, CC[A]] =
    new Factory[A, CC[A]] {
      def fromSpecific(it: IterableOnce[A]): CC[A] = factory.from[A](it)
      def newBuilder(): Builder[A, CC[A]] = factory.newBuilder[A]()
    }

  implicit def toBuildFrom[Ev[_], A: Ev, CC[_]](factory: EvidenceIterableFactory[CC, Ev]): BuildFrom[Any, A, CC[A]] =
    new BuildFrom[Any, A, CC[A]] {
      def fromSpecificIterable(from: Any)(it: Iterable[A]): CC[A] = factory.from[A](it)
      def newBuilder(from: Any): Builder[A, CC[A]] = factory.newBuilder[A]()
    }

  class Delegate[CC[_], Ev[_]](delegate: EvidenceIterableFactory[CC, Ev]) extends EvidenceIterableFactory[CC, Ev] {
    def empty[A : Ev]: CC[A] = delegate.empty
    def from[E : Ev](it: IterableOnce[E]): CC[E] = delegate.from(it)
    def newBuilder[A : Ev](): Builder[A, CC[A]] = delegate.newBuilder[A]()
  }
}

/** Base trait for companion objects of collections that require an implicit `Ordering`.
  * @tparam CC Collection type constructor (e.g. `SortedSet`)
  */
trait SortedIterableFactory[+CC[_]] extends EvidenceIterableFactory[CC, Ordering]

object SortedIterableFactory {
  class Delegate[CC[_]](delegate: EvidenceIterableFactory[CC, Ordering])
    extends EvidenceIterableFactory.Delegate[CC, Ordering](delegate) with SortedIterableFactory[CC]
}

/** Base trait for companion objects of collections that require an implicit `ClassTag`.
  * @tparam CC Collection type constructor (e.g. `ImmutableArray`)
  */
trait ClassTagIterableFactory[+CC[_]] extends EvidenceIterableFactory[CC, ClassTag] {

  @`inline` private[this] implicit def ccClassTag[X]: ClassTag[CC[X]] =
    ClassTag.AnyRef.asInstanceOf[ClassTag[CC[X]]] // Good enough for boxed vs primitive arrays

  /** Produces a $coll containing repeated applications of a function to a start value.
    *
    *  @param start the start value of the $coll
    *  @param len   the number of elements contained in the $coll
    *  @param f     the function that's repeatedly applied
    *  @return      a $coll with `len` values in the sequence `start, f(start), f(f(start)), ...`
    */
  def iterate[A : ClassTag](start: A, len: Int)(f: A => A): CC[A] = from(new View.Iterate(start, len)(f))

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
    *  @param   n3  the number of elements in the 3nd dimension
    *  @param   elem the element computation
    *  @return  A $coll that contains the results of `n1 x n2 x n3` evaluations of `elem`.
    */
  def fill[A : ClassTag](n1: Int, n2: Int, n3: Int)(elem: => A): CC[CC[CC[A]] @uncheckedVariance] = fill(n1)(fill(n2, n3)(elem))

  /** Produces a four-dimensional $coll containing the results of some element computation a number of times.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   n3  the number of elements in the 3nd dimension
    *  @param   n4  the number of elements in the 4th dimension
    *  @param   elem the element computation
    *  @return  A $coll that contains the results of `n1 x n2 x n3 x n4` evaluations of `elem`.
    */
  def fill[A : ClassTag](n1: Int, n2: Int, n3: Int, n4: Int)(elem: => A): CC[CC[CC[CC[A]]] @uncheckedVariance] =
    fill(n1)(fill(n2, n3, n4)(elem))

  /** Produces a five-dimensional $coll containing the results of some element computation a number of times.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   n3  the number of elements in the 3nd dimension
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
    *  @param   n3  the number of elements in the 3nd dimension
    *  @param   f   The function computing element values
    *  @return A $coll consisting of elements `f(i1, i2, i3)`
    *          for `0 <= i1 < n1`, `0 <= i2 < n2`, and `0 <= i3 < n3`.
    */
  def tabulate[A : ClassTag](n1: Int, n2: Int, n3: Int)(f: (Int, Int, Int) => A): CC[CC[CC[A]] @uncheckedVariance] =
    tabulate(n1)(i1 => tabulate(n2, n3)(f(i1, _, _)))

  /** Produces a four-dimensional $coll containing values of a given function over ranges of integer values starting from 0.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   n3  the number of elements in the 3nd dimension
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
    *  @param   n3  the number of elements in the 3nd dimension
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
  class Delegate[CC[_]](delegate: EvidenceIterableFactory[CC, ClassTag])
    extends EvidenceIterableFactory.Delegate[CC, ClassTag](delegate) with ClassTagIterableFactory[CC]

  /** An IterableFactory that uses ClassTag.Any as the evidence for every element type. This may or may not be
    * sound depending on the use of the `ClassTag` by the collection implementation. */
  class AnyIterableDelegate[CC[_]](delegate: ClassTagIterableFactory[CC]) extends IterableFactory[CC] {
    def empty[A]: CC[A] = delegate.empty(ClassTag.Any).asInstanceOf[CC[A]]
    def from[A](it: IterableOnce[A]): CC[A] = delegate.from[Any](it)(ClassTag.Any).asInstanceOf[CC[A]]
    def newBuilder[A](): Builder[A, CC[A]] = delegate.newBuilder()(ClassTag.Any).asInstanceOf[Builder[A, CC[A]]]
    override def apply[A](elems: A*): CC[A] = delegate.apply[Any](elems: _*)(ClassTag.Any).asInstanceOf[CC[A]]
    override def iterate[A](start: A, len: Int)(f: A => A): CC[A] = delegate.iterate[A](start, len)(f)(ClassTag.Any.asInstanceOf[ClassTag[A]])
    override def range[A](start: A, end: A)(implicit i: Integral[A]): CC[A] = delegate.range[A](start, end)(i, ClassTag.Any.asInstanceOf[ClassTag[A]])
    override def range[A](start: A, end: A, step: A)(implicit i: Integral[A]): CC[A] = delegate.range[A](start, end, step)(i, ClassTag.Any.asInstanceOf[ClassTag[A]])
    override def fill[A](n: Int)(elem: => A): CC[A] = delegate.fill[Any](n)(elem)(ClassTag.Any).asInstanceOf[CC[A]]
    override def tabulate[A](n: Int)(f: Int => A): CC[A] = delegate.tabulate[Any](n)(f)(ClassTag.Any).asInstanceOf[CC[A]]
  }
}

/**
  * @tparam CC Collection type constructor (e.g. `WrappedArray`)
  */
trait ClassTagSeqFactory[+CC[_]] extends ClassTagIterableFactory[CC] {
  def unapplySeq[A](x: CC[A] @uncheckedVariance): Some[CC[A]] = Some(x) //TODO is uncheckedVariance sound here?
}

object ClassTagSeqFactory {
  class Delegate[CC[_]](delegate: ClassTagSeqFactory[CC])
    extends ClassTagIterableFactory.Delegate[CC](delegate) with ClassTagSeqFactory[CC]

  /** A SeqFactory that uses ClassTag.Any as the evidence for every element type. This may or may not be
    * sound depending on the use of the `ClassTag` by the collection implementation. */
  class AnySeqDelegate[CC[_]](delegate: ClassTagSeqFactory[CC])
    extends ClassTagIterableFactory.AnyIterableDelegate[CC](delegate) with SeqFactory[CC]
}

trait StrictOptimizedClassTagSeqFactory[+CC[_]] extends ClassTagSeqFactory[CC] {

  override def fill[A : ClassTag](n: Int)(elem: => A): CC[A] = {
    val b = newBuilder[A]()
    b.sizeHint(n)
    var i = 0
    while (i < n) {
      b += elem
      i += 1
    }
    b.result()
  }

  override def tabulate[A : ClassTag](n: Int)(f: Int => A): CC[A] = {
    val b = newBuilder[A]()
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
  *   @author Martin Odersky
  *   @version 2.8
  *
  * @define coll collection
  * @define Coll `Iterable`
  */
trait SortedMapFactory[+CC[_, _]] {

  def empty[K : Ordering, V]: CC[K, V]

  def from[K : Ordering, V](it: IterableOnce[(K, V)]): CC[K, V]

  def apply[K : Ordering, V](elems: (K, V)*): CC[K, V] = from(elems)

  def newBuilder[K : Ordering, V](): Builder[(K, V), CC[K, V]]

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
  implicit def toFactory[K : Ordering, V, CC[_, _]](factory: SortedMapFactory[CC]): Factory[(K, V), CC[K, V]] =
    new Factory[(K, V), CC[K, V]] {
      def fromSpecific(it: IterableOnce[(K, V)]): CC[K, V] = factory.from[K, V](it)
      def newBuilder(): Builder[(K, V), CC[K, V]] = factory.newBuilder[K, V]()
    }

  implicit def toBuildFrom[K : Ordering, V, CC[_, _]](factory: SortedMapFactory[CC]): BuildFrom[Any, (K, V), CC[K, V]] =
    new BuildFrom[Any, (K, V), CC[K, V]] {
      def fromSpecificIterable(from: Any)(it: Iterable[(K, V)]) = factory.from(it)
      def newBuilder(from: Any) = factory.newBuilder[K, V]()
    }

  class Delegate[CC[_, _]](delegate: SortedMapFactory[CC]) extends SortedMapFactory[CC] {
    def from[K : Ordering, V](it: IterableOnce[(K, V)]): CC[K, V] = delegate.from(it)
    def empty[K : Ordering, V]: CC[K, V] = delegate.empty
    def newBuilder[K : Ordering, V](): Builder[(K, V), CC[K, V]] = delegate.newBuilder()
  }
}
