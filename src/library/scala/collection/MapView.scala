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

package scala.collection

import scala.annotation.nowarn
import scala.collection.MapView.SomeMapOps
import scala.collection.mutable.Builder

trait MapView[K, +V]
  extends MapOps[K, V, ({ type l[X, Y] = View[(X, Y)] })#l, View[(K, V)]]
    with View[(K, V)] {

  override def view: MapView[K, V] = this

  // Ideally this returns a `View`, but bincompat
  /** Creates a view over all keys of this map.
   *
   *  @return the keys of this map as a view.
   */
  @nowarn("msg=overriding method keys")
  override def keys: Iterable[K] = new MapView.Keys(this)

  // Ideally this returns a `View`, but bincompat
  /** Creates a view over all values of this map.
   *
   *  @return the values of this map as a view.
   */
  override def values: Iterable[V] = new MapView.Values(this)

  /** Filters this map by retaining only keys satisfying a predicate.
    *  @param  p   the predicate used to test keys
    *  @return an immutable map consisting only of those key value pairs of this map where the key satisfies
    *          the predicate `p`. The resulting map wraps the original map without copying any elements.
    */
  override def filterKeys(p: K => Boolean): MapView[K, V] = new MapView.FilterKeys(this, p)

  /** Transforms this map by applying a function to every retrieved value.
    *  @param  f   the function used to transform values of this map.
    *  @return a map view which maps every key of this map
    *          to `f(this(key))`. The resulting map wraps the original map without copying any elements.
    */
  override def mapValues[W](f: V => W): MapView[K, W] = new MapView.MapValues(this, f)

  override def filter(pred: ((K, V)) => Boolean): MapView[K, V] = new MapView.Filter(this, isFlipped = false, pred)

  override def filterNot(pred: ((K, V)) => Boolean): MapView[K, V] = new MapView.Filter(this, isFlipped = true, pred)

  override def partition(p: ((K, V)) => Boolean): (MapView[K, V], MapView[K, V]) = (filter(p), filterNot(p))

  override def tapEach[U](f: ((K, V)) => U): MapView[K, V] = new MapView.TapEach(this, f)

  def mapFactory: MapViewFactory = MapView

  override def empty: MapView[K, V] = mapFactory.empty

  override def withFilter(p: ((K, V)) => Boolean): MapOps.WithFilter[K, V, View, ({ type l[X, Y] = View[(X, Y)] })#l] = new MapOps.WithFilter(this, p)

  override def toString: String = super[View].toString

  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
  override protected[this] def stringPrefix: String = "MapView"
}

object MapView extends MapViewFactory {

  /** An `IterableOps` whose collection type and collection type constructor are unknown */
  type SomeIterableConstr[X, Y] = IterableOps[_, AnyConstr, _]
  /** A `MapOps` whose collection type and collection type constructor are (mostly) unknown */
  type SomeMapOps[K, +V] = MapOps[K, V, SomeIterableConstr, _]

  @SerialVersionUID(3L)
  private val EmptyMapView: MapView[Any, Nothing] = new AbstractMapView[Any, Nothing] {
    override def get(key: Any): Option[Nothing] = None
    override def iterator: Iterator[Nothing] = Iterator.empty[Nothing]
    override def knownSize: Int = 0
    override def isEmpty: Boolean = true
    override def filterKeys(p: Any => Boolean): MapView[Any, Nothing] = this
    override def mapValues[W](f: Nothing => W): MapView[Any, Nothing] = this
    override def filter(pred: ((Any, Nothing)) => Boolean): MapView[Any, Nothing] = this
    override def filterNot(pred: ((Any, Nothing)) => Boolean): MapView[Any, Nothing] = this
    override def partition(p: ((Any, Nothing)) => Boolean): (MapView[Any, Nothing], MapView[Any, Nothing]) = (this, this)
  }

  @SerialVersionUID(3L)
  class Id[K, +V](underlying: SomeMapOps[K, V]) extends AbstractMapView[K, V] {
    def get(key: K): Option[V] = underlying.get(key)
    def iterator: Iterator[(K, V)] = underlying.iterator
    override def knownSize: Int = underlying.knownSize
    override def isEmpty: Boolean = underlying.isEmpty
  }

  // Ideally this is public, but bincompat
  @SerialVersionUID(3L)
  private class Keys[K](underlying: SomeMapOps[K, _]) extends AbstractView[K] {
    def iterator: Iterator[K] = underlying.keysIterator
    override def knownSize: Int = underlying.knownSize
    override def isEmpty: Boolean = underlying.isEmpty
  }

  // Ideally this is public, but bincompat
  @SerialVersionUID(3L)
  private class Values[+V](underlying: SomeMapOps[_, V]) extends AbstractView[V] {
    def iterator: Iterator[V] = underlying.valuesIterator
    override def knownSize: Int = underlying.knownSize
    override def isEmpty: Boolean = underlying.isEmpty
  }

  @SerialVersionUID(3L)
  class MapValues[K, +V, +W](underlying: SomeMapOps[K, V], f: V => W) extends AbstractMapView[K, W] {
    def iterator: Iterator[(K, W)] = underlying.iterator.map(kv => (kv._1, f(kv._2)))
    def get(key: K): Option[W] = underlying.get(key).map(f)
    override def knownSize: Int = underlying.knownSize
    override def isEmpty: Boolean = underlying.isEmpty
  }

  @SerialVersionUID(3L)
  class FilterKeys[K, +V](underlying: SomeMapOps[K, V], p: K => Boolean) extends AbstractMapView[K, V] {
    def iterator: Iterator[(K, V)] = underlying.iterator.filter { case (k, _) => p(k) }
    def get(key: K): Option[V] = if (p(key)) underlying.get(key) else None
    override def knownSize: Int = if (underlying.knownSize == 0) 0 else super.knownSize
    override def isEmpty: Boolean = iterator.isEmpty
  }

  @SerialVersionUID(3L)
  class Filter[K, +V](underlying: SomeMapOps[K, V], isFlipped: Boolean, p: ((K, V)) => Boolean) extends AbstractMapView[K, V] {
    def iterator: Iterator[(K, V)] = underlying.iterator.filterImpl(p, isFlipped)
    def get(key: K): Option[V] = underlying.get(key) match {
      case s @ Some(v) if p((key, v)) != isFlipped => s
      case _ => None
    }
    override def knownSize: Int = if (underlying.knownSize == 0) 0 else super.knownSize
    override def isEmpty: Boolean = iterator.isEmpty
  }

  @SerialVersionUID(3L)
  class TapEach[K, +V, +U](underlying: SomeMapOps[K, V], f: ((K, V)) => U) extends AbstractMapView[K, V] {
    override def get(key: K): Option[V] = {
      underlying.get(key) match {
        case s @ Some(v) =>
          f((key, v))
          s
        case None => None
      }
    }
    override def iterator: Iterator[(K, V)] = underlying.iterator.tapEach(f)
    override def knownSize: Int = underlying.knownSize
    override def isEmpty: Boolean = underlying.isEmpty
  }

  override def newBuilder[X, Y]: Builder[(X, Y), MapView[X, Y]] = mutable.HashMap.newBuilder[X, Y].mapResult(_.view)

  override def empty[K, V]: MapView[K, V] = EmptyMapView.asInstanceOf[MapView[K, V]]

  override def from[K, V](it: IterableOnce[(K, V)]): View[(K, V)] = View.from(it)

  override def from[K, V](it: SomeMapOps[K, V]): MapView[K, V] = it match {
    case mv: MapView[K, V] => mv
    case other => new MapView.Id(other)
  }

  override def apply[K, V](elems: (K, V)*): MapView[K, V] = from(elems.toMap)
}

trait MapViewFactory extends collection.MapFactory[({ type l[X, Y] = View[(X, Y)]})#l] {

  def newBuilder[X, Y]: Builder[(X, Y), MapView[X, Y]]

  def empty[X, Y]: MapView[X, Y]

  def from[K, V](it: SomeMapOps[K, V]): MapView[K, V]

  override def apply[K, V](elems: (K, V)*): MapView[K, V] = from(elems.toMap)
}

/** Explicit instantiation of the `MapView` trait to reduce class file size in subclasses. */
@SerialVersionUID(3L)
abstract class AbstractMapView[K, +V] extends AbstractView[(K, V)] with MapView[K, V]

