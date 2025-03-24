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
package immutable

import scala.annotation.{nowarn, tailrec}

/** This class implements immutable maps using a vector/map-based data structure, which preserves insertion order.
  *
  *  Unlike `ListMap`, `VectorMap` has amortized effectively constant lookup at the expense
  *  of using extra memory and generally lower performance for other operations
  *
  *  @tparam K      the type of the keys contained in this vector map.
  *  @tparam V      the type of the values associated with the keys in this vector map.
  *
  * @define coll immutable vector map
  * @define Coll `immutable.VectorMap`
  */
final class VectorMap[K, +V] private (
    private[immutable] val fields: Vector[Any], // K | Tombstone | Null
    private[immutable] val underlying: Map[K, (Int, V)],
    dropped: Int)
  extends AbstractMap[K, V]
    with SeqMap[K, V]
    with StrictOptimizedMapOps[K, V, VectorMap, VectorMap[K, V]]
    with MapFactoryDefaults[K, V, VectorMap, Iterable] {

  import VectorMap._

  override protected[this] def className: String = "VectorMap"

  private[immutable] def this(fields: Vector[K], underlying: Map[K, (Int, V)]) = this(fields, underlying, 0)

  override val size = underlying.size

  override def knownSize: Int = size

  override def isEmpty: Boolean = size == 0

  def updated[V1 >: V](key: K, value: V1): VectorMap[K, V1] = {
    underlying.get(key) match {
      case Some((slot, _)) =>
        new VectorMap(fields, underlying.updated[(Int, V1)](key, (slot, value)), dropped)
      case None =>
        new VectorMap(fields :+ key, underlying.updated[(Int, V1)](key, (fields.length + dropped, value)), dropped)
    }
  }

  override def withDefault[V1 >: V](d: K => V1): Map[K, V1] =
    new Map.WithDefault(this, d)

  override def withDefaultValue[V1 >: V](d: V1): Map[K, V1] =
    new Map.WithDefault[K, V1](this, _ => d)

  def get(key: K): Option[V] = underlying.get(key) match {
    case Some(v) => Some(v._2)
    case None    => None
  }

  @tailrec
  private def nextValidField(slot: Int): (Int, K) = {
    if (slot >= fields.size) (-1, null.asInstanceOf[K])
    else fields(slot) match {
      case Tombstone(distance) => nextValidField(slot + distance)
      case k /*: K | Null */   => (slot, k.asInstanceOf[K])
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
        nextValidField(nextSlot) match {
          case (-1, _) =>
            slot = fieldsLength
            key = null.asInstanceOf[K]
          case (s, k) =>
            slot = s
            key = k
        }
      }
    }

    advance()

    override def hasNext: Boolean = slot < fieldsLength

    override def next(): (K, V) =
      if (!hasNext) Iterator.empty.next()
      else {
        val result = (key, underlying(key)._2)
        advance()
        result
      }
  }

  // No-Op overrides to allow for more efficient steppers in a minor release.
  // Refining the return type to `S with EfficientSplit` is binary compatible.

  override def stepper[S <: Stepper[_]](implicit shape: StepperShape[(K, V), S]): S = super.stepper(shape)

  override def keyStepper[S <: Stepper[_]](implicit shape: StepperShape[K, S]): S = super.keyStepper(shape)

  override def valueStepper[S <: Stepper[_]](implicit shape: StepperShape[V, S]): S = super.valueStepper(shape)


  def removed(key: K): VectorMap[K, V] = {
    if (isEmpty) empty
    else {
      var fs = fields
      val sz = fs.size
      underlying.get(key) match {
        case Some(_) if size == 1 => empty
        case Some((slot, _)) =>
          val s = slot - dropped

          // Calculate next of kin
          val next =
            if (s < sz - 1) fs(s + 1) match {
              case Tombstone(d) => s + d + 1
              case _ => s + 1
            } else s + 1

          fs = fs.updated(s, Tombstone(next - s))

          // Calculate first index of preceding tombstone sequence
          val first =
            if (s > 0) {
              fs(s - 1) match {
                case Tombstone(d) if d < 0 => if (s + d >= 0) s + d else 0
                case Tombstone(d) if d == 1 => s - 1
                case Tombstone(d) => throw new IllegalStateException("tombstone indicate wrong position: " + d)
                case _ => s
              }
            }else s
          fs = fs.updated(first, Tombstone(next - first))

          // Calculate last index of succeeding tombstone sequence
          val last = next - 1
          if (last != first) {
            fs = fs.updated(last, Tombstone(first - 1 - last))
          }
          new VectorMap(fs, underlying - key, dropped)
        case _ =>
          this
      }
    }
  }

  override def mapFactory: MapFactory[VectorMap] = VectorMap

  override def contains(key: K): Boolean = underlying.contains(key)

  override def head: (K, V) = iterator.next()

  override def last: (K, V) = {
    if (isEmpty) throw new UnsupportedOperationException("empty.last")
    val lastSlot = fields.length - 1
    val last = fields.last match {
      case Tombstone(d) if d < 0 => fields(lastSlot + d).asInstanceOf[K]
      case Tombstone(d) if d == 1 => fields(lastSlot - 1).asInstanceOf[K]
      case Tombstone(d) => throw new IllegalStateException("tombstone indicate wrong position: " + d)
      case k => k.asInstanceOf[K]
    }
    (last, underlying(last)._2)
  }

  override def lastOption: Option[(K, V)] = {
    if (isEmpty) None
    else Some(last)
  }

  override def tail: VectorMap[K, V] = {
    if (isEmpty) throw new UnsupportedOperationException("empty.tail")
    val (slot, key) = nextValidField(0)
    new VectorMap(fields.drop(slot + 1), underlying - key, dropped + slot + 1)
  }

  override def init: VectorMap[K, V] = {
    if (isEmpty) throw new UnsupportedOperationException("empty.init")
    val lastSlot = fields.size - 1
    val (slot, key) = fields.last match {
      case Tombstone(d) if d < 0 => (lastSlot + d, fields(lastSlot + d).asInstanceOf[K])
      case Tombstone(d) if d == 1 => (lastSlot - 1, fields(lastSlot - 1).asInstanceOf[K])
      case Tombstone(d) => throw new IllegalStateException("tombstone indicate wrong position: " + d)
      case k => (lastSlot, k.asInstanceOf[K])
    }
    new VectorMap(fields.dropRight(fields.size - slot), underlying - key, dropped)
  }

  /** A [[Vector]] of the keys contained by this map.
   *
   *  @return  a [[Vector]] of the keys contained by this map.
   */
  @nowarn("msg=overriding method keys")
  override def keys: Vector[K] = keysIterator.toVector

  override def values: Iterable[V] = new Iterable[V] with IterableFactoryDefaults[V, Iterable] {
    override def iterator: Iterator[V] = keysIterator.map(underlying(_)._2)
  }
}

object VectorMap extends MapFactory[VectorMap] {
  //Class to mark deleted slots in 'fields'.
  //When one or more consecutive slots are deleted, the 'distance' of the first 'Tombstone'
  // represents the distance to the location of the next undeleted slot (or the last slot in 'fields' +1 if it does not exist).
  //When two or more consecutive slots are deleted, the 'distance' of the trailing 'Tombstone'
  // represents the distance to the location of the previous undeleted slot ( or -1 if it does not exist) multiplied by -1.
  //For other deleted slots, it simply indicates that they have been deleted.
  private[VectorMap] final case class Tombstone(distance: Int)

  private[this] final val EmptyMap: VectorMap[Nothing, Nothing] =
    new VectorMap[Nothing, Nothing](Vector.empty[Nothing], HashMap.empty[Nothing, (Int, Nothing)])

  def empty[K, V]: VectorMap[K, V] = EmptyMap.asInstanceOf[VectorMap[K, V]]

  def from[K, V](it: collection.IterableOnce[(K, V)]): VectorMap[K, V] =
    it match {
      case vm: VectorMap[K, V] => vm
      case _                   => (newBuilder[K, V] ++= it).result()
    }

  def newBuilder[K, V]: mutable.Builder[(K, V), VectorMap[K, V]] = new VectorMapBuilder[K, V]
}

private[immutable] final class VectorMapBuilder[K, V] extends mutable.Builder[(K, V), VectorMap[K, V]] {
  private[this] val vectorBuilder = new VectorBuilder[K]
  private[this] val mapBuilder = new MapBuilderImpl[K, (Int, V)]
  private[this] var aliased: VectorMap[K, V] = _

  override def clear(): Unit = {
    vectorBuilder.clear()
    mapBuilder.clear()
    aliased = null
  }

  override def result(): VectorMap[K, V] = {
    if (aliased eq null) {
      aliased = new VectorMap(vectorBuilder.result(), mapBuilder.result())
    }
    aliased
  }
  def addOne(key: K, value: V): this.type = {
    if (aliased ne null) {
      aliased = aliased.updated(key, value)
    } else {
      mapBuilder.getOrElse(key, null) match {
        case (slot, _) =>
          mapBuilder.addOne(key, (slot, value))
        case null =>
          val vectorSize = vectorBuilder.size
          vectorBuilder.addOne(key)
          mapBuilder.addOne(key, (vectorSize, value))
      }
    }
    this
  }

  override def addOne(elem: (K, V)): this.type = addOne(elem._1, elem._2)
}
