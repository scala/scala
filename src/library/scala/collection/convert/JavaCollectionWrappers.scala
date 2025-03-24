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
package convert

import java.util.{concurrent => juc}
import java.util.{NavigableMap}
import java.{lang => jl, util => ju}

import scala.jdk.CollectionConverters._
import scala.util.Try
import scala.util.chaining._
import scala.util.control.ControlThrowable

/** Wrappers for exposing Scala collections as Java collections and vice-versa */
@SerialVersionUID(3L)
// not private[convert] because `WeakHashMap` uses JMapWrapper
private[collection] object JavaCollectionWrappers extends Serializable {
  @SerialVersionUID(3L)
  class IteratorWrapper[A](val underlying: Iterator[A]) extends ju.Iterator[A] with ju.Enumeration[A] with Serializable {
    def hasNext = underlying.hasNext
    def next() = underlying.next()
    def hasMoreElements = underlying.hasNext
    def nextElement() = underlying.next()
    override def remove(): Nothing = throw new UnsupportedOperationException
    override def equals(other: Any): Boolean = other match {
      case that: IteratorWrapper[_] => this.underlying == that.underlying
      case _ => false
    }
    override def hashCode: Int = underlying.hashCode()
  }

  @SerialVersionUID(3L)
  class JIteratorWrapper[A](val underlying: ju.Iterator[A]) extends AbstractIterator[A] with Serializable {
    def hasNext = underlying.hasNext
    def next() = underlying.next
    override def equals(other: Any): Boolean = other match {
      case that: JIteratorWrapper[_] => this.underlying == that.underlying
      case _ => false
    }
    override def hashCode: Int = underlying.hashCode()
  }

  @SerialVersionUID(3L)
  class JEnumerationWrapper[A](val underlying: ju.Enumeration[A]) extends AbstractIterator[A] with Serializable {
    def hasNext = underlying.hasMoreElements
    def next() = underlying.nextElement
    override def equals(other: Any): Boolean = other match {
      case that: JEnumerationWrapper[_] => this.underlying == that.underlying
      case _ => false
    }
    override def hashCode: Int = underlying.hashCode()
  }

  trait IterableWrapperTrait[A] extends ju.AbstractCollection[A] {
    val underlying: Iterable[A]
    def size = underlying.size
    override def iterator: IteratorWrapper[A] = new IteratorWrapper(underlying.iterator)
    override def isEmpty = underlying.isEmpty
  }

  @SerialVersionUID(3L)
  class IterableWrapper[A](val underlying: Iterable[A]) extends ju.AbstractCollection[A] with IterableWrapperTrait[A] with Serializable {
    override def equals(other: Any): Boolean = other match {
      case that: IterableWrapper[_] => this.underlying == that.underlying
      case _ => false
    }
    override def hashCode: Int = underlying.hashCode()
  }

  @SerialVersionUID(3L)
  class JIterableWrapper[A](val underlying: jl.Iterable[A])
    extends AbstractIterable[A]
      with StrictOptimizedIterableOps[A, Iterable, Iterable[A]]
      with Serializable {
    def iterator = underlying.iterator.asScala
    override def iterableFactory: mutable.ArrayBuffer.type = mutable.ArrayBuffer
    override def isEmpty: Boolean = !underlying.iterator().hasNext
    override def equals(other: Any): Boolean = other match {
      case that: JIterableWrapper[_] => this.underlying == that.underlying
      case _ => false
    }
    override def hashCode: Int = underlying.hashCode()
  }

  @SerialVersionUID(3L)
  class JCollectionWrapper[A](val underlying: ju.Collection[A])
    extends AbstractIterable[A]
      with StrictOptimizedIterableOps[A, Iterable, Iterable[A]]
      with Serializable {
    def iterator = underlying.iterator.asScala
    override def size = underlying.size
    override def knownSize: Int = if (underlying.isEmpty) 0 else super.knownSize
    override def isEmpty = underlying.isEmpty
    override def iterableFactory: mutable.ArrayBuffer.type = mutable.ArrayBuffer
    override def equals(other: Any): Boolean = other match {
      case that: JCollectionWrapper[_] => this.underlying == that.underlying
      case _ => false
    }
    override def hashCode: Int = underlying.hashCode()
  }

  @SerialVersionUID(3L)
  class SeqWrapper[A](val underlying: Seq[A]) extends ju.AbstractList[A] with IterableWrapperTrait[A] with Serializable {
    def get(i: Int) = underlying(i)
  }

  @SerialVersionUID(3L)
  class MutableSeqWrapper[A](val underlying: mutable.Seq[A]) extends ju.AbstractList[A] with IterableWrapperTrait[A] with Serializable {
    def get(i: Int) = underlying(i)
    override def set(i: Int, elem: A) = {
      val p = underlying(i)
      underlying(i) = elem
      p
    }
  }

  @SerialVersionUID(3L)
  class MutableBufferWrapper[A](val underlying: mutable.Buffer[A]) extends ju.AbstractList[A] with IterableWrapperTrait[A] with Serializable {
    def get(i: Int) = underlying(i)
    override def set(i: Int, elem: A) = { val p = underlying(i); underlying(i) = elem; p }
    override def add(elem: A) = { underlying += elem; true }
    override def remove(i: Int) = underlying remove i
  }

  @SerialVersionUID(3L)
  class JListWrapper[A](val underlying: ju.List[A])
    extends mutable.AbstractBuffer[A]
      with SeqOps[A, mutable.Buffer, mutable.Buffer[A]]
      with StrictOptimizedSeqOps[A, mutable.Buffer, mutable.Buffer[A]]
      with IterableFactoryDefaults[A, mutable.Buffer]
      with Serializable {
    def length = underlying.size
    override def knownSize: Int = if (underlying.isEmpty) 0 else super.knownSize
    override def isEmpty = underlying.isEmpty
    override def iterator: Iterator[A] = underlying.iterator.asScala
    def apply(i: Int) = underlying.get(i)
    def update(i: Int, elem: A) = underlying.set(i, elem)
    def prepend(elem: A) = { underlying.subList(0, 0) add elem; this }
    def addOne(elem: A): this.type = { underlying add elem; this }
    def insert(idx: Int,elem: A): Unit = underlying.subList(0, idx).add(elem)
    def insertAll(i: Int, elems: IterableOnce[A]) = {
      val ins = underlying.subList(0, i)
      elems.iterator.foreach(ins.add(_))
    }
    def remove(i: Int) = underlying.remove(i)
    def clear() = underlying.clear()
    // Note: Clone cannot just call underlying.clone because in Java, only specific collections
    // expose clone methods.  Generically, they're protected.
    override def clone(): JListWrapper[A] = new JListWrapper(new ju.ArrayList[A](underlying))
    def patchInPlace(from: Int, patch: scala.collection.IterableOnce[A], replaced: Int): this.type = {
      remove(from, replaced)
      insertAll(from, patch)
      this
    }
    def remove(from: Int, n: Int): Unit = underlying.subList(from, from+n).clear()
    override def iterableFactory: mutable.ArrayBuffer.type = mutable.ArrayBuffer
    override def subtractOne(elem: A): this.type = { underlying.remove(elem.asInstanceOf[AnyRef]); this }
  }

  @SerialVersionUID(3L)
  class SetWrapper[A](underlying: Set[A]) extends ju.AbstractSet[A] with Serializable { self =>
    // Note various overrides to avoid performance gotchas.
    override def contains(o: Object): Boolean = {
      try { underlying.contains(o.asInstanceOf[A]) }
      catch { case cce: ClassCastException => false }
    }
    override def isEmpty = underlying.isEmpty
    def size = underlying.size
    def iterator: ju.Iterator[A] = new ju.Iterator[A] {
      val ui = underlying.iterator
      var prev: Option[A] = None
      def hasNext = ui.hasNext
      def next = { val e = ui.next(); prev = Some(e); e }
      override def remove() = prev match {
        case Some(e) =>
          underlying match {
            case ms: mutable.Set[a] =>
              ms remove e
              prev = None
            case _ =>
              throw new UnsupportedOperationException("remove")
          }
        case _ =>
          throw new IllegalStateException("next must be called at least once before remove")
      }
    }
  }

  @SerialVersionUID(3L)
  class MutableSetWrapper[A](val underlying: mutable.Set[A]) extends SetWrapper[A](underlying) with Serializable {
    override def add(elem: A) = {
      val sz = underlying.size
      underlying += elem
      sz < underlying.size
    }
    override def remove(elem: AnyRef) =
      try underlying.remove(elem.asInstanceOf[A])
      catch { case ex: ClassCastException => false }
    override def clear() = underlying.clear()
  }

  @SerialVersionUID(3L)
  class JSetWrapper[A](val underlying: ju.Set[A])
    extends mutable.AbstractSet[A]
      with mutable.SetOps[A, mutable.Set, mutable.Set[A]]
      with StrictOptimizedSetOps[A, mutable.Set, mutable.Set[A]]
      with Serializable {

    override def size: Int = underlying.size
    override def isEmpty: Boolean = underlying.isEmpty
    override def knownSize: Int = if (underlying.isEmpty) 0 else super.knownSize
    def iterator: Iterator[A] = underlying.iterator.asScala

    def contains(elem: A): Boolean = underlying.contains(elem)

    def addOne(elem: A): this.type = { underlying add elem; this }
    def subtractOne(elem: A): this.type = { underlying remove elem; this }

    override def remove(elem: A): Boolean = underlying remove elem

    override def clear(): Unit = {
      underlying.clear()
    }

    override def empty: mutable.Set[A] = new JSetWrapper(new ju.HashSet[A])

    // Note: Clone cannot just call underlying.clone because in Java, only specific collections
    // expose clone methods.  Generically, they're protected.
    override def clone(): mutable.Set[A] = new JSetWrapper[A](new ju.LinkedHashSet[A](underlying))

    override def iterableFactory: IterableFactory[mutable.Set] = mutable.HashSet

    override def filterInPlace(p: A => Boolean): this.type = {
      if (underlying.size() > 0) underlying.removeIf(!p(_))
      this
    }
  }

  @SerialVersionUID(3L)
  class MapWrapper[K, V](underlying: Map[K, V]) extends ju.AbstractMap[K, V] with Serializable { self =>
    override def size = underlying.size

    override def get(key: AnyRef): V = try {
      underlying get key.asInstanceOf[K] match {
        case None => null.asInstanceOf[V]
        case Some(v) => v
      }
    } catch {
      case ex: ClassCastException => null.asInstanceOf[V]
    }

    override def entrySet: ju.Set[ju.Map.Entry[K, V]] = new ju.AbstractSet[ju.Map.Entry[K, V]] {
      def size = self.size

      def iterator: ju.Iterator[ju.Map.Entry[K, V]] = new ju.Iterator[ju.Map.Entry[K, V]] {
        val ui = underlying.iterator
        var prev : Option[K] = None

        def hasNext = ui.hasNext

        def next(): ju.Map.Entry[K, V] = {
          val (k, v) = ui.next()
          prev = Some(k)
          new ju.Map.Entry[K, V] {
            def getKey = k
            def getValue = v
            def setValue(v1 : V) = self.put(k, v1)

            // It's important that this implementation conform to the contract
            // specified in the javadocs of java.util.Map.Entry.hashCode
            //
            // See https://github.com/scala/bug/issues/10663
            override def hashCode = {
              (if (k == null) 0 else k.hashCode()) ^
              (if (v == null) 0 else v.hashCode())
            }

            override def equals(other: Any) = other match {
              case e: ju.Map.Entry[_, _] => k == e.getKey && v == e.getValue
              case _ => false
            }
          }
        }

        override def remove(): Unit = {
          prev match {
            case Some(k) =>
              underlying match {
                case mm: mutable.Map[a, _] =>
                  mm -= k
                  prev = None
                case _ =>
                  throw new UnsupportedOperationException("remove")
              }
            case _ =>
              throw new IllegalStateException("next must be called at least once before remove")
          }
        }
      }
    }

    override def containsKey(key: AnyRef): Boolean = try {
      // Note: Subclass of collection.Map with specific key type may redirect generic
      // contains to specific contains, which will throw a ClassCastException if the
      // wrong type is passed. This is why we need a type cast to A inside a try/catch.
      underlying.contains(key.asInstanceOf[K])
    } catch {
      case ex: ClassCastException => false
    }
  }

  @SerialVersionUID(3L)
  class MutableMapWrapper[K, V](val underlying: mutable.Map[K, V]) extends MapWrapper[K, V](underlying) {
    override def put(k: K, v: V) = underlying.put(k, v) match {
      case Some(v1) => v1
      case None => null.asInstanceOf[V]
    }

    override def remove(k: AnyRef): V = try {
      underlying remove k.asInstanceOf[K] match {
        case None => null.asInstanceOf[V]
        case Some(v) => v
      }
    } catch {
      case ex: ClassCastException => null.asInstanceOf[V]
    }

    override def clear() = underlying.clear()
  }

  @SerialVersionUID(3L)
  abstract class AbstractJMapWrapper[K, V]
    extends mutable.AbstractMap[K, V]
      with JMapWrapperLike[K, V, mutable.Map, mutable.Map[K, V]] with Serializable

  trait JMapWrapperLike[K, V, +CC[X, Y] <: mutable.MapOps[X, Y, CC, _], +C <: mutable.MapOps[K, V, CC, C]]
    extends mutable.MapOps[K, V, CC, C]
      with StrictOptimizedMapOps[K, V, CC, C]
      with StrictOptimizedIterableOps[(K, V), mutable.Iterable, C] {

    def underlying: ju.Map[K, V]

    override def size = underlying.size

    // support Some(null) if currently bound to null
    def get(k: K) = {
      val v = underlying.get(k)
      if (v != null)
        Some(v)
      else if (underlying.containsKey(k))
        Some(null.asInstanceOf[V])
      else
        None
    }

    override def getOrElseUpdate(key: K, op: => V): V =
      underlying.computeIfAbsent(key, _ => op) match {
        case null => update(key, null.asInstanceOf[V]); null.asInstanceOf[V]
        case v    => v
      }

    def addOne(kv: (K, V)): this.type = { underlying.put(kv._1, kv._2); this }
    def subtractOne(key: K): this.type = { underlying remove key; this }

    // support Some(null) if currently bound to null
    override def put(k: K, v: V): Option[V] =
      if (v == null) {
        val present = underlying.containsKey(k)
        val result  = underlying.put(k, v)
        if (present) Some(result) else None
      } else {
        var result: Option[V] = None
        def recompute(k0: K, v0: V): V = v.tap(_ =>
          if (v0 != null) result = Some(v0)
          else if (underlying.containsKey(k0)) result = Some(null.asInstanceOf[V])
        )
        underlying.compute(k, recompute)
        result
      }

    override def update(k: K, v: V): Unit = underlying.put(k, v)

    override def updateWith(key: K)(remappingFunction: Option[V] => Option[V]): Option[V] = {
      def remap(k: K, v: V): V =
        remappingFunction(Option(v)) match {
          case Some(null) => throw PutNull
          case Some(x)    => x
          case None       => null.asInstanceOf[V]
        }
      try Option(underlying.compute(key, remap))
      catch {
        case PutNull => update(key, null.asInstanceOf[V]); Some(null.asInstanceOf[V])
      }
    }

    // support Some(null) if currently bound to null
    override def remove(k: K): Option[V] = {
      var result: Option[V] = None
      def recompute(k0: K, v0: V): V = {
        if (v0 != null) result = Some(v0)
        else if (underlying.containsKey(k0)) result = Some(null.asInstanceOf[V])
        null.asInstanceOf[V]
      }
      underlying.compute(k, recompute)
      result
    }

    def iterator: Iterator[(K, V)] = new AbstractIterator[(K, V)] {
      val ui = underlying.entrySet.iterator
      def hasNext = ui.hasNext
      def next() = { val e = ui.next(); (e.getKey, e.getValue) }
    }

    override def foreachEntry[U](f: (K, V) => U): Unit = {
      val i = underlying.entrySet().iterator()
      while (i.hasNext) {
        val entry = i.next()
        f(entry.getKey, entry.getValue)
      }
    }

    override def clear() = underlying.clear()

  }

  /** Wraps a Java map as a Scala one.  If the map is to support concurrent access,
    * use [[JConcurrentMapWrapper]] instead.  If the wrapped map is synchronized
    * (e.g. from `java.util.Collections.synchronizedMap`), it is your responsibility
    * to wrap all non-atomic operations with `underlying.synchronized`.
    * This includes `get`, as `java.util.Map`'s API does not allow for an
    * atomic `get` when `null` values may be present.
    */
  @SerialVersionUID(3L)
  class JMapWrapper[K, V](val underlying : ju.Map[K, V])
    extends AbstractJMapWrapper[K, V] with Serializable {

    override def isEmpty: Boolean = underlying.isEmpty
    override def knownSize: Int = if (underlying.isEmpty) 0 else super.knownSize
    override def empty: JMapWrapper[K, V] = new JMapWrapper(new ju.HashMap[K, V])
  }

  @SerialVersionUID(3L)
  class ConcurrentMapWrapper[K, V](underlying: concurrent.Map[K, V]) extends MutableMapWrapper[K, V](underlying) with juc.ConcurrentMap[K, V] {

    def underlyingConcurrentMap: concurrent.Map[K, V] = underlying

    override def putIfAbsent(k: K, v: V) = underlying.putIfAbsent(k, v).getOrElse(null.asInstanceOf[V])

    override def remove(k: AnyRef, v: AnyRef) =
      try underlying.remove(k.asInstanceOf[K], v.asInstanceOf[V])
      catch { case ex: ClassCastException => false }

    override def replace(k: K, v: V): V = underlying.replace(k, v).getOrElse(null.asInstanceOf[V])

    override def replace(k: K, oldval: V, newval: V) = underlying.replace(k, oldval, newval)
  }

  /** Wraps a concurrent Java map as a Scala one.  Single-element concurrent
   *  access is supported; multi-element operations such as maps and filters
   *  are not guaranteed to be atomic.
   */
  @SerialVersionUID(3L)
  class JConcurrentMapWrapper[K, V](val underlying: juc.ConcurrentMap[K, V])
    extends AbstractJMapWrapper[K, V]
      with concurrent.Map[K, V] {

    override def get(k: K) = Option(underlying get k)

    override def getOrElseUpdate(key: K, op: => V): V =
      underlying.computeIfAbsent(key, _ => op) match {
        case null => super/*[concurrent.Map]*/.getOrElseUpdate(key, op)
        case v    => v
      }

    override def isEmpty: Boolean = underlying.isEmpty
    override def knownSize: Int = if (underlying.isEmpty) 0 else super.knownSize
    override def empty: JConcurrentMapWrapper[K, V] = new JConcurrentMapWrapper(new juc.ConcurrentHashMap[K, V])

    def putIfAbsent(k: K, v: V): Option[V] = Option(underlying.putIfAbsent(k, v))

    def remove(k: K, v: V): Boolean = underlying.remove(k, v)

    def replace(k: K, v: V): Option[V] = Option(underlying.replace(k, v))

    def replace(k: K, oldvalue: V, newvalue: V): Boolean = underlying.replace(k, oldvalue, newvalue)

    override def lastOption: Option[(K, V)] =
      underlying match {
        case nav: NavigableMap[K @unchecked, V @unchecked] => Option(nav.lastEntry).map(e => (e.getKey, e.getValue))
        case _ if isEmpty => None
        case _ => Try(last).toOption
      }

    override def updateWith(key: K)(remappingFunction: Option[V] => Option[V]): Option[V] = {
      def remap(k: K, v: V): V =
        remappingFunction(Option(v)) match {
          case Some(null) => throw PutNull // see scala/scala#10129
          case Some(x)    => x
          case None       => null.asInstanceOf[V]
        }
      try Option(underlying.compute(key, remap))
      catch {
        case PutNull => super/*[concurrent.Map]*/.updateWith(key)(remappingFunction)
      }
    }
  }

  @SerialVersionUID(3L)
  class DictionaryWrapper[K, V](val underlying: mutable.Map[K, V]) extends ju.Dictionary[K, V] with Serializable {
    def size: Int = underlying.size
    def isEmpty: Boolean = underlying.isEmpty
    def keys: ju.Enumeration[K] = underlying.keysIterator.asJavaEnumeration
    def elements: ju.Enumeration[V] = underlying.valuesIterator.asJavaEnumeration
    def get(key: AnyRef) = try {
      underlying get key.asInstanceOf[K] match {
        case None => null.asInstanceOf[V]
        case Some(v) => v
      }
    } catch {
      case ex: ClassCastException => null.asInstanceOf[V]
    }
    def put(key: K, value: V): V = underlying.put(key, value) match {
      case Some(v) => v
      case None => null.asInstanceOf[V]
    }
    override def remove(key: AnyRef) = try {
      underlying remove key.asInstanceOf[K] match {
        case None => null.asInstanceOf[V]
        case Some(v) => v
      }
    } catch {
      case ex: ClassCastException => null.asInstanceOf[V]
    }

    override def equals(other: Any): Boolean = other match {
      case that: DictionaryWrapper[_, _] => this.underlying == that.underlying
      case _ => false
    }

    override def hashCode: Int = underlying.hashCode()
  }

  @SerialVersionUID(3L)
  class JDictionaryWrapper[K, V](val underlying: ju.Dictionary[K, V]) extends mutable.AbstractMap[K, V] with Serializable {
    override def size: Int = underlying.size
    override def isEmpty: Boolean = underlying.isEmpty
    override def knownSize: Int = if (underlying.isEmpty) 0 else super.knownSize

    def get(k: K) = Option(underlying get k)

    def addOne(kv: (K, V)): this.type = { underlying.put(kv._1, kv._2); this }
    def subtractOne(key: K): this.type = { underlying remove key; this }

    override def put(k: K, v: V): Option[V] = Option(underlying.put(k, v))

    override def update(k: K, v: V): Unit = { underlying.put(k, v) }

    override def remove(k: K): Option[V] = Option(underlying remove k)
    def iterator = underlying.keys.asScala map (k => (k, underlying get k))

    override def clear() = iterator.foreach(entry => underlying.remove(entry._1))

    override def mapFactory: mutable.HashMap.type = mutable.HashMap
  }

  @SerialVersionUID(3L)
  class JPropertiesWrapper(underlying: ju.Properties)
    extends mutable.AbstractMap[String, String]
      with mutable.MapOps[String, String, mutable.Map, mutable.Map[String, String]]
      with StrictOptimizedMapOps[String, String, mutable.Map, mutable.Map[String, String]]
      with StrictOptimizedIterableOps[(String, String), mutable.Iterable, mutable.Map[String, String]]
      with Serializable {

    override def size = underlying.size
    override def isEmpty: Boolean = underlying.isEmpty
    override def knownSize: Int = size
    def get(k: String) = {
      val v = underlying get k
      if (v != null) Some(v.asInstanceOf[String]) else None
    }

    def addOne(kv: (String, String)): this.type = { underlying.put(kv._1, kv._2); this }
    def subtractOne(key: String): this.type = { underlying remove key; this }

    override def put(k: String, v: String): Option[String] = {
      val r = underlying.put(k, v)
      if (r != null) Some(r.asInstanceOf[String]) else None
    }

    override def update(k: String, v: String): Unit = { underlying.put(k, v) }

    override def remove(k: String): Option[String] = {
      val r = underlying remove k
      if (r != null) Some(r.asInstanceOf[String]) else None
    }

    def iterator: Iterator[(String, String)] = new AbstractIterator[(String, String)] {
      val ui = underlying.entrySet.iterator
      def hasNext = ui.hasNext
      def next() = {
        val e = ui.next()
        (e.getKey.asInstanceOf[String], e.getValue.asInstanceOf[String])
      }
    }

    override def clear() = underlying.clear()

    override def empty: JPropertiesWrapper = new JPropertiesWrapper(new ju.Properties)

    def getProperty(key: String) = underlying.getProperty(key)

    def getProperty(key: String, defaultValue: String) =
      underlying.getProperty(key, defaultValue)

    def setProperty(key: String, value: String) =
      underlying.setProperty(key, value)

    override def mapFactory: mutable.HashMap.type = mutable.HashMap
  }

  /** Thrown when certain Map operations attempt to put a null value. */
  private val PutNull = new ControlThrowable {}
}
