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

import scala.collection.mutable.{Builder, ReusableBuilder}

/** A base trait for ordered, immutable maps.
 *
 *  Note that the [[equals]] method for [[SeqMap]] compares key-value pairs
 *  without regard to ordering.
 *
 *  All behavior is defined in terms of the abstract methods in `SeqMap`.
 *  It is sufficient for concrete subclasses to implement those methods.
 *  Methods that return a new map, in particular [[removed]] and [[updated]], must preserve ordering.
 *
 *  @tparam K      the type of the keys contained in this linked map.
 *  @tparam V      the type of the values associated with the keys in this linked map.
 *
 *  @define coll immutable seq map
 *  @define Coll `immutable.SeqMap`
 */

trait SeqMap[K, +V]
  extends Map[K, V]
    with collection.SeqMap[K, V]
    with MapOps[K, V, SeqMap, SeqMap[K, V]]
    with MapFactoryDefaults[K, V, SeqMap, Iterable] {
  override def mapFactory: MapFactory[SeqMap] = SeqMap
}


object SeqMap extends MapFactory[SeqMap] {
  def empty[K, V]: SeqMap[K, V] = EmptySeqMap.asInstanceOf[SeqMap[K, V]]

  def from[K, V](it: collection.IterableOnce[(K, V)]): SeqMap[K, V] =
    it match {
      //case sm: SeqMap[K, V] => sm
      case m: ListMap[K, V]    => m
      case m: TreeSeqMap[K, V] => m
      case m: VectorMap[K, V]  => m
      case m: SeqMap1[K, V]    => m
      case m: SeqMap2[K, V]    => m
      case m: SeqMap3[K, V]    => m
      case m: SeqMap4[K, V]    => m
      case it: Iterable[_] if it.isEmpty => empty[K, V]
      case _ => (newBuilder[K, V] ++= it).result()
    }

  def newBuilder[K, V]: Builder[(K, V), SeqMap[K, V]] = new SeqMapBuilderImpl

  @SerialVersionUID(3L)
  private object EmptySeqMap extends SeqMap[Any, Nothing] with Serializable {
    override def size: Int = 0
    override def knownSize: Int = 0
    override def apply(key: Any) = throw new NoSuchElementException("key not found: " + key)
    override def contains(key: Any) = false
    def get(key: Any): Option[Nothing] = None
    override def getOrElse [V1](key: Any, default: => V1): V1 = default
    def iterator: Iterator[(Any, Nothing)] = Iterator.empty
    def updated [V1] (key: Any, value: V1): SeqMap[Any, V1] = new SeqMap1(key, value)
    def removed(key: Any): SeqMap[Any, Nothing] = this
  }

  @SerialVersionUID(3L)
  private[immutable] final class SeqMap1[K, +V](key1: K, value1: V) extends SeqMap[K,V] with Serializable {
    override def size: Int = 1
    override def knownSize: Int = 1
    override def apply(key: K) = if (key == key1) value1 else throw new NoSuchElementException("key not found: " + key)
    override def contains(key: K) = key == key1
    def get(key: K): Option[V] =
      if (key == key1) Some(value1) else None
    override def getOrElse [V1 >: V](key: K, default: => V1): V1 =
      if (key == key1) value1 else default
    def iterator = Iterator.single((key1, value1))
    def updated[V1 >: V](key: K, value: V1): SeqMap[K, V1] =
      if (key == key1) new SeqMap1(key1, value)
      else new SeqMap2(key1, value1, key, value)
    def removed(key: K): SeqMap[K, V] =
      if (key == key1) SeqMap.empty else this
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1))
    }
    override def foreachEntry[U](f: (K, V) => U): Unit = {
      f(key1, value1)
    }
  }

  @SerialVersionUID(3L)
  private[immutable] final class SeqMap2[K, +V](key1: K, value1: V, key2: K, value2: V) extends SeqMap[K,V] with Serializable {
    override def size: Int = 2
    override def knownSize: Int = 2
    override def apply(key: K) =
      if (key == key1) value1
      else if (key == key2) value2
      else throw new NoSuchElementException("key not found: " + key)
    override def contains(key: K) = (key == key1) || (key == key2)
    def get(key: K): Option[V] =
      if (key == key1) Some(value1)
      else if (key == key2) Some(value2)
      else None
    override def getOrElse [V1 >: V](key: K, default: => V1): V1 =
      if (key == key1) value1
      else if (key == key2) value2
      else default
    def iterator = ((key1, value1) :: (key2, value2) :: Nil).iterator
    def updated[V1 >: V](key: K, value: V1): SeqMap[K, V1] =
      if (key == key1) new SeqMap2(key1, value, key2, value2)
      else if (key == key2) new SeqMap2(key1, value1, key2, value)
      else new SeqMap3(key1, value1, key2, value2, key, value)
    def removed(key: K): SeqMap[K, V] =
      if (key == key1) new SeqMap1(key2, value2)
      else if (key == key2) new SeqMap1(key1, value1)
      else this
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1)); f((key2, value2))
    }
    override def foreachEntry[U](f: (K, V) => U): Unit = {
      f(key1, value1)
      f(key2, value2)
    }
  }

  @SerialVersionUID(3L)
  private[immutable] class SeqMap3[K, +V](key1: K, value1: V, key2: K, value2: V, key3: K, value3: V) extends SeqMap[K,V] with Serializable {
    override def size: Int = 3
    override def knownSize: Int = 3
    override def apply(key: K) =
      if (key == key1) value1
      else if (key == key2) value2
      else if (key == key3) value3
      else throw new NoSuchElementException("key not found: " + key)
    override def contains(key: K) = (key == key1) || (key == key2) || (key == key3)
    def get(key: K): Option[V] =
      if (key == key1) Some(value1)
      else if (key == key2) Some(value2)
      else if (key == key3) Some(value3)
      else None
    override def getOrElse [V1 >: V](key: K, default: => V1): V1 =
      if (key == key1) value1
      else if (key == key2) value2
      else if (key == key3) value3
      else default
    def iterator = ((key1, value1) :: (key2, value2) :: (key3, value3) :: Nil).iterator
    def updated[V1 >: V](key: K, value: V1): SeqMap[K, V1] =
      if (key == key1)      new SeqMap3(key1, value, key2, value2, key3, value3)
      else if (key == key2) new SeqMap3(key1, value1, key2, value, key3, value3)
      else if (key == key3) new SeqMap3(key1, value1, key2, value2, key3, value)
      else new SeqMap4(key1, value1, key2, value2, key3, value3, key, value)
    def removed(key: K): SeqMap[K, V] =
      if (key == key1)      new SeqMap2(key2, value2, key3, value3)
      else if (key == key2) new SeqMap2(key1, value1, key3, value3)
      else if (key == key3) new SeqMap2(key1, value1, key2, value2)
      else this
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1)); f((key2, value2)); f((key3, value3))
    }
    override def foreachEntry[U](f: (K, V) => U): Unit = {
      f(key1, value1)
      f(key2, value2)
      f(key3, value3)
    }
  }

  @SerialVersionUID(3L)
  private[immutable] final class SeqMap4[K, +V](key1: K, value1: V, key2: K, value2: V, key3: K, value3: V, key4: K, value4: V) extends SeqMap[K,V] with Serializable {
    override def size: Int = 4
    override def knownSize: Int = 4
    override def apply(key: K) =
      if (key == key1) value1
      else if (key == key2) value2
      else if (key == key3) value3
      else if (key == key4) value4
      else throw new NoSuchElementException("key not found: " + key)
    override def contains(key: K) = (key == key1) || (key == key2) || (key == key3) || (key == key4)
    def get(key: K): Option[V] =
      if (key == key1) Some(value1)
      else if (key == key2) Some(value2)
      else if (key == key3) Some(value3)
      else if (key == key4) Some(value4)
      else None
    override def getOrElse [V1 >: V](key: K, default: => V1): V1 =
      if (key == key1) value1
      else if (key == key2) value2
      else if (key == key3) value3
      else if (key == key4) value4
      else default
    def iterator = ((key1, value1) :: (key2, value2) :: (key3, value3) :: (key4, value4) :: Nil).iterator
    def updated[V1 >: V](key: K, value: V1): SeqMap[K, V1] =
      if (key == key1)      new SeqMap4(key1, value, key2, value2, key3, value3, key4, value4)
      else if (key == key2) new SeqMap4(key1, value1, key2, value, key3, value3, key4, value4)
      else if (key == key3) new SeqMap4(key1, value1, key2, value2, key3, value, key4, value4)
      else if (key == key4) new SeqMap4(key1, value1, key2, value2, key3, value3, key4, value)
      else {
        // Directly create the elements for performance reasons
        val fields = Vector(key1, key2, key3, key4, key)
        val underlying: Map[K, (Int, V1)] =
          HashMap(
            (key1, (0, value1)),
            (key2, (1, value2)),
            (key3, (2, value3)),
            (key4, (3, value4)),
            (key, (4, value))
          )
        new VectorMap(fields, underlying)
      }
    def removed(key: K): SeqMap[K, V] =
      if (key == key1)      new SeqMap3(key2, value2, key3, value3, key4, value4)
      else if (key == key2) new SeqMap3(key1, value1, key3, value3, key4, value4)
      else if (key == key3) new SeqMap3(key1, value1, key2, value2, key4, value4)
      else if (key == key4) new SeqMap3(key1, value1, key2, value2, key3, value3)
      else this
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1)); f((key2, value2)); f((key3, value3)); f((key4, value4))
    }
    override def foreachEntry[U](f: (K, V) => U): Unit = {
      f(key1, value1)
      f(key2, value2)
      f(key3, value3)
      f(key4, value4)
    }

    private[SeqMap] def buildTo[V1 >: V](builder: Builder[(K, V1), SeqMap[K, V1]]): builder.type =
      builder.addOne((key1, value1)).addOne((key2, value2)).addOne((key3, value3)).addOne((key4, value4))
  }

  private final class SeqMapBuilderImpl[K, V] extends ReusableBuilder[(K, V), SeqMap[K, V]] {
    private[this] var elems: SeqMap[K, V] = SeqMap.empty
    private[this] var switchedToVectorMapBuilder: Boolean = false
    private[this] var vectorMapBuilder: VectorMapBuilder[K, V] = _

    override def clear(): Unit = {
      elems = SeqMap.empty
      if (vectorMapBuilder != null) {
        vectorMapBuilder.clear()
      }
      switchedToVectorMapBuilder = false
    }

    override def result(): SeqMap[K, V] =
      if (switchedToVectorMapBuilder) vectorMapBuilder.result() else elems

    def addOne(elem: (K, V)) = {
      if (switchedToVectorMapBuilder) {
        vectorMapBuilder.addOne(elem)
      } else if (elems.size < 4) {
        elems = elems + elem
      } else {
        // assert(elems.size == 4)
        if (elems.contains(elem._1)) {
          elems = elems + elem // will not increase the size of the map
        } else {
          switchedToVectorMapBuilder = true
          if (vectorMapBuilder == null) {
            vectorMapBuilder = new VectorMapBuilder
          }
          elems.asInstanceOf[SeqMap4[K, V]].buildTo(vectorMapBuilder)
          vectorMapBuilder.addOne(elem)
        }
      }

      this
    }

    override def addAll(xs: IterableOnce[(K, V)]): this.type =
      if (switchedToVectorMapBuilder) {
        vectorMapBuilder.addAll(xs)
        this
      } else {
        super.addAll(xs)
      }
  }
}
