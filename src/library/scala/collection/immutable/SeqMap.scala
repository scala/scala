package scala
package collection
package immutable

import java.io.{ObjectInputStream, ObjectOutputStream}

import scala.collection.mutable.Builder

/**
  * A generic trait for ordered immutable maps. Concrete classes have to provide
  * functionality for the abstract methods in `SeqMap`.
  *
  * Note that when checking for equality [[SeqMap]] does not take into account
  * ordering.
  *
  * @tparam K      the type of the keys contained in this linked map.
  * @tparam V      the type of the values associated with the keys in this linked map.
  *
  * @author Matthew de Detrich
  * @version 2.13
  * @since 2.13
  * @define coll immutable seq map
  * @define Coll `immutable.SeqMap`
  */

trait SeqMap[K, +V]
  extends AbstractMap[K, V]
    with MapOps[K, V, SeqMap, SeqMap[K, V]]

object SeqMap extends MapFactory[SeqMap] {
  def empty[K, V]: SeqMap[K, V] = EmptyLinkedMap.asInstanceOf[SeqMap[K, V]]

  def from[K, V](it: collection.IterableOnce[(K, V)]): SeqMap[K, V] =
    it match {
      case sm: SeqMap[K, V] => sm
      case _ => (newBuilder[K, V] ++= it).result()
    }

  def newBuilder[K, V]: Builder[(K, V), SeqMap[K, V]] = VectorMap.newBuilder

  @SerialVersionUID(3L)
  private object EmptyLinkedMap extends SeqMap[Any, Nothing] {
    override def size: Int = 0
    override def knownSize: Int = 0
    override def apply(key: Any) = throw new NoSuchElementException("key not found: " + key)
    override def contains(key: Any) = false
    def get(key: Any): Option[Nothing] = None
    override def getOrElse [V1](key: Any, default: => V1): V1 = default
    def iterator: Iterator[(Any, Nothing)] = Iterator.empty
    def updated [V1] (key: Any, value: V1): SeqMap[Any, V1] = new SeqMap1(key, value)
    def remove(key: Any): SeqMap[Any, Nothing] = this
  }

  @SerialVersionUID(3L)
  private final class SeqMap1[K, +V](key1: K, value1: V) extends SeqMap[K,V] with Serializable {
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
    def remove(key: K): SeqMap[K, V] =
      if (key == key1) SeqMap.empty else this
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1))
    }
  }

  @SerialVersionUID(3L)
  private final class SeqMap2[K, +V](key1: K, value1: V, key2: K, value2: V) extends SeqMap[K,V] with Serializable {
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
    def remove(key: K): SeqMap[K, V] =
      if (key == key1) new SeqMap1(key2, value2)
      else if (key == key2) new SeqMap1(key1, value1)
      else this
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1)); f((key2, value2))
    }
  }

  @SerialVersionUID(3L)
  private class SeqMap3[K, +V](key1: K, value1: V, key2: K, value2: V, key3: K, value3: V) extends SeqMap[K,V] with Serializable {
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
    def remove(key: K): SeqMap[K, V] =
      if (key == key1)      new SeqMap2(key2, value2, key3, value3)
      else if (key == key2) new SeqMap2(key1, value1, key3, value3)
      else if (key == key3) new SeqMap2(key1, value1, key2, value2)
      else this
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1)); f((key2, value2)); f((key3, value3))
    }
  }

  @SerialVersionUID(3L)
  private final class SeqMap4[K, +V](key1: K, value1: V, key2: K, value2: V, key3: K, value3: V, key4: K, value4: V) extends SeqMap[K,V] with Serializable {
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
        val underlying: Map[K, (Int, V1)] = if (useBaseline)
          OldHashMap(
            (key1, (0, value1)),
            (key2, (1, value2)),
            (key3, (2, value3)),
            (key4, (3, value4)),
            (key, (4, value))
          )
        else
          HashMap(
            (key1, (0, value1)),
            (key2, (1, value2)),
            (key3, (2, value3)),
            (key4, (3, value4)),
            (key, (4, value))
          )
        new VectorMap(fields, underlying)
      }
    def remove(key: K): SeqMap[K, V] =
      if (key == key1)      new SeqMap3(key2, value2, key3, value3, key4, value4)
      else if (key == key2) new SeqMap3(key1, value1, key3, value3, key4, value4)
      else if (key == key3) new SeqMap3(key1, value1, key2, value2, key4, value4)
      else if (key == key4) new SeqMap3(key1, value1, key2, value2, key3, value3)
      else this
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1)); f((key2, value2)); f((key3, value3)); f((key4, value4))
    }
  }

  // getenv not getProperty for Scala.js friendliness.
  // TODO remove before 2.13.0-RC1? see scala/collection-strawman#572
  private final val useBaseline: Boolean =
  System.getenv("SCALA_COLLECTION_IMMUTABLE_USE_BASELINE") == "true"

  // scalac generates a `readReplace` method to discard the deserialized state (see https://github.com/scala/bug/issues/10412).
  // This prevents it from serializing it in the first place:
  private[this] def writeObject(out: ObjectOutputStream): Unit = ()
  private[this] def readObject(in: ObjectInputStream): Unit = ()

}
