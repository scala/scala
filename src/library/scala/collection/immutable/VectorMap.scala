/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package immutable

import scala.annotation.tailrec

/** This class implements immutable maps using a vector/map-based data structure, which preserves insertion order.
  *
  *  Unlike `ListMap`, `VectorMap` has amortized effectively constant lookup at the expense
  *  of using extra memory and generally lower performance for other operations
  *
  *  @tparam K      the type of the keys contained in this vector map.
  *  @tparam V      the type of the values associated with the keys in this vector map.
  *
  * @author Matthew de Detrich
  * @author Odd Möller
  * @version 2.13
  * @since 2.13
  * @define coll immutable vector map
  * @define Coll `immutable.VectorMap`
  */
final class VectorMap[K, +V] private (
    private[immutable] val fields: Vector[Any],
    private[immutable] val underlying: Map[K, (Int, V)], dummy: Boolean)
  extends AbstractMap[K, V]
    with SeqMap[K, V]
    with StrictOptimizedMapOps[K, V, VectorMap, VectorMap[K, V]] {

  import VectorMap._

  override protected[this] def className: String = "VectorMap"

  private[immutable] def this(fields: Vector[K], underlying: Map[K, (Int, V)]) = {
    this(fields, underlying, false)
  }

  override val size = underlying.size

  override def knownSize: Int = size

  override def isEmpty: Boolean = size == 0

  def updated[V1 >: V](key: K, value: V1): VectorMap[K, V1] = {
    underlying.get(key) match {
      case Some((slot, _)) =>
        new VectorMap(fields, underlying.updated[(Int, V1)](key, (slot, value)), false)
      case None =>
        new VectorMap(fields :+ key, underlying.updated[(Int, V1)](key, (fields.length, value)), false)
    }
  }

  override def withDefault[V1 >: V](d: K => V1): Map.WithDefault[K, V1] =
    new Map.WithDefault(this, d)

  override def withDefaultValue[V1 >: V](d: V1): Map.WithDefault[K, V1] =
    new Map.WithDefault[K, V1](this, _ => d)

  def get(key: K): Option[V] = underlying.get(key) match {
    case Some(v) => Some(v._2)
    case None    => None
  }

  @tailrec
  private def field(slot: Int): (Int, K) = {
    fields(slot) match {
      case Tombstone.Kinless =>
        (-1, null.asInstanceOf[K])
      case Tombstone.NextOfKin(distance) =>
        field(slot + distance)
      case k =>
        (slot, k.asInstanceOf[K])
    }
  }

  def iterator: Iterator[(K, V)] = new AbstractIterator[(K, V)] {
    private[this] val fieldsLength = fields.length
    private[this] var slot = -1
    private[this] var key: K = null.asInstanceOf[K]

    private[this] def advance(): Unit = {
      val nextSlot = slot + 1
      if (nextSlot >= fieldsLength) {
        slot = fieldsLength
        key = null.asInstanceOf[K]
      } else {
        field(nextSlot) match {
          case (-1, _) ⇒
            slot = fieldsLength
            key = null.asInstanceOf[K]
          case (s, k) ⇒
            slot = s
            key = k
        }
      }
    }

    advance()

    override def hasNext: Boolean = slot < fieldsLength

    override def next(): (K, V) = {
      if (!hasNext) throw new NoSuchElementException("next called on depleted iterator")
      val result = (key, underlying(key)._2)
      advance()
      result
    }
  }

  def removed(key: K): VectorMap[K, V] = {
    if (isEmpty) empty
    else {
      var fs = fields
      val sz = fs.size
      underlying.get(key) match {
        case Some(_) if size == 1 => empty
        case Some((slot, _)) =>
          val s = field(slot)._1
          // Calculate distance to next of kin
          val d =
            if (s < sz - 1) fs(s + 1) match {
              case Tombstone.Kinless => 0
              case Tombstone.NextOfKin(d) => d + 1
              case _ => 1
            } else 0
          fs = fs.updated(s, Tombstone(d))
          if (s > 0) {
            // Adjust distance to next of kin for all preceding tombstones
            var t = s - 1
            var prev = fs(t)
            while (t >= 0 && prev.isInstanceOf[Tombstone]) {
              fs = prev match {
                case Tombstone.Kinless => throw new IllegalStateException("kinless tombstone found in prefix: " + key)
                case Tombstone.NextOfKin(_) if d == 0 => fs.updated(t, Tombstone.Kinless)
                case Tombstone.NextOfKin(d) => fs.updated(t, Tombstone(d + 1))
                case _ => fs
              }
              t -= 1
              if (t >= 0) prev = fs(t)
            }
          }
          new VectorMap(fs, underlying - key, false)
        case _ =>
          this
      }
    }
  }

  override def mapFactory: MapFactory[VectorMap] = VectorMap

  override def contains(key: K): Boolean = underlying.contains(key)

  override def head: (K, V) = iterator.next()

  override def last: (K, V) = {
    val last = fields
      .reverseIterator
      .find(!_.isInstanceOf[Tombstone])
      .get
      .asInstanceOf[K]
    (last, underlying(last)._2)
  }

  override def lastOption: Option[(K, V)] = {
    fields
      .reverseIterator
      .find(!_.isInstanceOf[Tombstone])
      .map { f ⇒
        val last = f.asInstanceOf[K]
        (last, underlying(last)._2)
      }
  }

  override def tail: VectorMap[K, V] = {
    val (slot, key) = field(0)
    new VectorMap(fields.drop(slot + 1), underlying - key, false)
  }

  override def init: VectorMap[K, V] = {
    val lastSlot = size - 1
    val (slot, key) = field(lastSlot)
    new VectorMap(fields.dropRight(slot - lastSlot + 1), underlying - key, false)
  }

  override def keys: Vector[K] = keysIterator.toVector

  override def values: Iterable[V] = new Iterable[V] {
    override def iterator: Iterator[V] = keysIterator.map(underlying(_)._2)
  }
}

object VectorMap extends MapFactory[VectorMap] {
  private[VectorMap] sealed trait Tombstone
  private[VectorMap] object Tombstone {
    final case object Kinless extends Tombstone {
      override def toString = "⤞"
    }
    final case class NextOfKin private[Tombstone] (distance: Int) extends Tombstone {
      override def toString = "⥅" + distance
    }
    def apply(distance: Int): Tombstone =
      if (distance <= 0) Kinless
      else NextOfKin(distance)
  }

  def empty[K, V]: VectorMap[K, V] =
    new VectorMap[K, V](
      Vector.empty[K],
      HashMap.empty[K, (Int, V)])

  def from[K, V](it: collection.IterableOnce[(K, V)]): VectorMap[K, V] =
    it match {
      case vm: VectorMap[K, V] => vm
      case _                   => (newBuilder[K, V] ++= it).result()
    }

  def newBuilder[K, V]: mutable.Builder[(K, V), VectorMap[K, V]] =
    new mutable.ImmutableBuilder[(K, V), VectorMap[K, V]](empty) {
      def addOne(elem: (K, V)): this.type = { elems = elems + elem; this }
    }

}
