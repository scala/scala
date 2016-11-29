/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package convert

import java.{ lang => jl, util => ju }, java.util.{ concurrent => juc }
import WrapAsScala._
import WrapAsJava._

/** Adapters for Java/Scala collections API. */
private[collection] trait Wrappers {
  trait IterableWrapperTrait[A] extends ju.AbstractCollection[A] {
    val underlying: Iterable[A]
    def size = underlying.size
    override def iterator = IteratorWrapper(underlying.iterator)
    override def isEmpty = underlying.isEmpty
  }

  case class IteratorWrapper[A](underlying: Iterator[A]) extends ju.Iterator[A] with ju.Enumeration[A] {
    def hasNext = underlying.hasNext
    def next() = underlying.next()
    def hasMoreElements = underlying.hasNext
    def nextElement() = underlying.next()
    override def remove() = throw new UnsupportedOperationException
  }

  class ToIteratorWrapper[A](underlying : Iterator[A]) {
    def asJava = new IteratorWrapper(underlying)
  }

  case class JIteratorWrapper[A](underlying: ju.Iterator[A]) extends AbstractIterator[A] with Iterator[A] {
    def hasNext = underlying.hasNext
    def next() = underlying.next
  }

  case class JEnumerationWrapper[A](underlying: ju.Enumeration[A]) extends AbstractIterator[A] with Iterator[A] {
    def hasNext = underlying.hasMoreElements
    def next() = underlying.nextElement
  }

  case class IterableWrapper[A](underlying: Iterable[A]) extends ju.AbstractCollection[A] with IterableWrapperTrait[A] { }

  case class JIterableWrapper[A](underlying: jl.Iterable[A]) extends AbstractIterable[A] with Iterable[A] {
    def iterator = underlying.iterator
    def newBuilder[B] = new mutable.ArrayBuffer[B]
  }

  case class JCollectionWrapper[A](underlying: ju.Collection[A]) extends AbstractIterable[A] with Iterable[A] {
    def iterator = underlying.iterator
    override def size = underlying.size
    override def isEmpty = underlying.isEmpty
    def newBuilder[B] = new mutable.ArrayBuffer[B]
  }

  case class SeqWrapper[A](underlying: Seq[A]) extends ju.AbstractList[A] with IterableWrapperTrait[A] {
    def get(i: Int) = underlying(i)
  }

  case class MutableSeqWrapper[A](underlying: mutable.Seq[A]) extends ju.AbstractList[A] with IterableWrapperTrait[A] {
    def get(i: Int) = underlying(i)
    override def set(i: Int, elem: A) = {
      val p = underlying(i)
      underlying(i) = elem
      p
    }
  }

  case class MutableBufferWrapper[A](underlying: mutable.Buffer[A]) extends ju.AbstractList[A] with IterableWrapperTrait[A] {
    def get(i: Int) = underlying(i)
    override def set(i: Int, elem: A) = { val p = underlying(i); underlying(i) = elem; p }
    override def add(elem: A) = { underlying append elem; true }
    override def remove(i: Int) = underlying remove i
  }

  case class JListWrapper[A](underlying: ju.List[A]) extends mutable.AbstractBuffer[A] with mutable.Buffer[A] {
    def length = underlying.size
    override def isEmpty = underlying.isEmpty
    override def iterator: Iterator[A] = underlying.iterator
    def apply(i: Int) = underlying.get(i)
    def update(i: Int, elem: A) = underlying.set(i, elem)
    def +=:(elem: A) = { underlying.subList(0, 0) add elem; this }
    def +=(elem: A): this.type = { underlying add elem; this }
    def insertAll(i: Int, elems: Traversable[A]) = {
      val ins = underlying.subList(0, i)
      elems.seq.foreach(ins.add(_))
    }
    def remove(i: Int) = underlying.remove(i)
    def clear() = underlying.clear()
    def result = this
    // Note: Clone cannot just call underlying.clone because in Java, only specific collections
    // expose clone methods.  Generically, they're protected.
    override def clone(): JListWrapper[A] = JListWrapper(new ju.ArrayList[A](underlying))
  }

  @SerialVersionUID(1L)
  class SetWrapper[A](underlying: Set[A]) extends ju.AbstractSet[A] with Serializable { self =>
    // Note various overrides to avoid performance gotchas.
    override def contains(o: Object): Boolean = {
      try { underlying.contains(o.asInstanceOf[A]) }
      catch { case cce: ClassCastException => false }
    }
    override def isEmpty = underlying.isEmpty
    def size = underlying.size
    def iterator = new ju.Iterator[A] {
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

  case class MutableSetWrapper[A](underlying: mutable.Set[A]) extends SetWrapper[A](underlying) {
    override def add(elem: A) = {
      val sz = underlying.size
      underlying += elem
      sz < underlying.size
    }
    override def remove(elem: AnyRef) =
      try underlying remove elem.asInstanceOf[A]
      catch { case ex: ClassCastException => false }
    override def clear() = underlying.clear()
  }

  case class JSetWrapper[A](underlying: ju.Set[A]) extends mutable.AbstractSet[A] with mutable.Set[A] with mutable.SetLike[A, JSetWrapper[A]] {

    override def size = underlying.size

    def iterator = underlying.iterator

    def contains(elem: A): Boolean = underlying.contains(elem)

    def +=(elem: A): this.type = { underlying add elem; this }
    def -=(elem: A): this.type = { underlying remove elem; this }

    override def add(elem: A): Boolean = underlying add elem
    override def remove(elem: A): Boolean = underlying remove elem
    override def clear() = underlying.clear()

    override def empty = JSetWrapper(new ju.HashSet[A])
    // Note: Clone cannot just call underlying.clone because in Java, only specific collections
    // expose clone methods.  Generically, they're protected.
    override def clone() =
      new JSetWrapper[A](new ju.LinkedHashSet[A](underlying))
  }

  @SerialVersionUID(1L)
  class MapWrapper[A, B](underlying: Map[A, B]) extends ju.AbstractMap[A, B] with Serializable { self =>
    override def size = underlying.size

    override def get(key: AnyRef): B = try {
      underlying get key.asInstanceOf[A] match {
        case None => null.asInstanceOf[B]
        case Some(v) => v
      }
    } catch {
      case ex: ClassCastException => null.asInstanceOf[B]
    }

    override def entrySet: ju.Set[ju.Map.Entry[A, B]] = new ju.AbstractSet[ju.Map.Entry[A, B]] {
      def size = self.size

      def iterator = new ju.Iterator[ju.Map.Entry[A, B]] {
        val ui = underlying.iterator
        var prev : Option[A] = None

        def hasNext = ui.hasNext

        def next() = {
          val (k, v) = ui.next()
          prev = Some(k)
          new ju.Map.Entry[A, B] {
            import scala.util.hashing.byteswap32
            def getKey = k
            def getValue = v
            def setValue(v1 : B) = self.put(k, v1)
            override def hashCode = byteswap32(k.##) + (byteswap32(v.##) << 16)
            override def equals(other: Any) = other match {
              case e: ju.Map.Entry[_, _] => k == e.getKey && v == e.getValue
              case _ => false
            }
          }
        }

        override def remove() {
          prev match {
            case Some(k) =>
              underlying match {
                case mm: mutable.Map[a, _] =>
                  mm remove k
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
      underlying.contains(key.asInstanceOf[A])
    } catch {
      case ex: ClassCastException => false
    }
  }

  case class MutableMapWrapper[A, B](underlying: mutable.Map[A, B]) extends MapWrapper[A, B](underlying) {
    override def put(k: A, v: B) = underlying.put(k, v) match {
      case Some(v1) => v1
      case None => null.asInstanceOf[B]
    }

    override def remove(k: AnyRef): B = try {
      underlying remove k.asInstanceOf[A] match {
        case None => null.asInstanceOf[B]
        case Some(v) => v
      }
    } catch {
      case ex: ClassCastException => null.asInstanceOf[B]
    }

    override def clear() = underlying.clear()
  }

  trait JMapWrapperLike[A, B, +Repr <: mutable.MapLike[A, B, Repr] with mutable.Map[A, B]] extends mutable.Map[A, B] with mutable.MapLike[A, B, Repr] {
    def underlying: ju.Map[A, B]

    override def size = underlying.size

    def get(k: A) = {
      val v = underlying get k
      if (v != null)
        Some(v)
      else if (underlying containsKey k)
        Some(null.asInstanceOf[B])
      else
        None
    }

    def +=(kv: (A, B)): this.type = { underlying.put(kv._1, kv._2); this }
    def -=(key: A): this.type = { underlying remove key; this }

    override def put(k: A, v: B): Option[B] = Option(underlying.put(k, v))

    override def update(k: A, v: B) { underlying.put(k, v) }

    override def remove(k: A): Option[B] = Option(underlying remove k)

    def iterator: Iterator[(A, B)] = new AbstractIterator[(A, B)] {
      val ui = underlying.entrySet.iterator
      def hasNext = ui.hasNext
      def next() = { val e = ui.next(); (e.getKey, e.getValue) }
    }

    override def clear() = underlying.clear()

    override def empty: Repr = null.asInstanceOf[Repr]
  }

  /** Wraps a Java map as a Scala one.  If the map is to support concurrent access,
    * use [[JConcurrentMapWrapper]] instead.  If the wrapped map is synchronized 
    * (e.g. from `java.util.Collections.synchronizedMap`), it is your responsibility 
    * to wrap all non-atomic operations with `underlying.synchronized`.
    * This includes `get`, as `java.util.Map`'s API does not allow for an
    * atomic `get` when `null` values may be present.
    */
  case class JMapWrapper[A, B](underlying : ju.Map[A, B]) extends mutable.AbstractMap[A, B] with JMapWrapperLike[A, B, JMapWrapper[A, B]] {
    override def empty = JMapWrapper(new ju.HashMap[A, B])
  }

  class ConcurrentMapWrapper[A, B](override val underlying: concurrent.Map[A, B]) extends MutableMapWrapper[A, B](underlying) with juc.ConcurrentMap[A, B] {

    override def putIfAbsent(k: A, v: B) = underlying.putIfAbsent(k, v) match {
      case Some(v) => v
      case None => null.asInstanceOf[B]
    }

    override def remove(k: AnyRef, v: AnyRef) = try {
      underlying.remove(k.asInstanceOf[A], v.asInstanceOf[B])
    } catch {
      case ex: ClassCastException =>
        false
    }

    override def replace(k: A, v: B): B = underlying.replace(k, v) match {
      case Some(v) => v
      case None => null.asInstanceOf[B]
    }

    override def replace(k: A, oldval: B, newval: B) = underlying.replace(k, oldval, newval)
  }

  /** Wraps a concurrent Java map as a Scala one.  Single-element concurrent
    * access is supported; multi-element operations such as maps and filters
    * are not guaranteed to be atomic.
    */
  case class JConcurrentMapWrapper[A, B](underlying: juc.ConcurrentMap[A, B]) extends mutable.AbstractMap[A, B] with JMapWrapperLike[A, B, JConcurrentMapWrapper[A, B]] with concurrent.Map[A, B] {
    override def get(k: A) = Option(underlying get k)

    override def empty = new JConcurrentMapWrapper(new juc.ConcurrentHashMap[A, B])

    def putIfAbsent(k: A, v: B): Option[B] = Option(underlying.putIfAbsent(k, v))

    def remove(k: A, v: B): Boolean = underlying.remove(k, v)

    def replace(k: A, v: B): Option[B] = Option(underlying.replace(k, v))

    def replace(k: A, oldvalue: B, newvalue: B): Boolean =
      underlying.replace(k, oldvalue, newvalue)
  }

  case class DictionaryWrapper[A, B](underlying: mutable.Map[A, B]) extends ju.Dictionary[A, B] {
    def size: Int = underlying.size
    def isEmpty: Boolean = underlying.isEmpty
    def keys: ju.Enumeration[A] = asJavaEnumeration(underlying.keysIterator)
    def elements: ju.Enumeration[B] = asJavaEnumeration(underlying.valuesIterator)
    def get(key: AnyRef) = try {
      underlying get key.asInstanceOf[A] match {
        case None => null.asInstanceOf[B]
        case Some(v) => v
      }
    } catch {
      case ex: ClassCastException => null.asInstanceOf[B]
    }
    def put(key: A, value: B): B = underlying.put(key, value) match {
      case Some(v) => v
      case None => null.asInstanceOf[B]
    }
    override def remove(key: AnyRef) = try {
      underlying remove key.asInstanceOf[A] match {
        case None => null.asInstanceOf[B]
        case Some(v) => v
      }
    } catch {
      case ex: ClassCastException => null.asInstanceOf[B]
    }
  }

  case class JDictionaryWrapper[A, B](underlying: ju.Dictionary[A, B]) extends mutable.AbstractMap[A, B] with mutable.Map[A, B] {
    override def size: Int = underlying.size

    def get(k: A) = Option(underlying get k)

    def +=(kv: (A, B)): this.type = { underlying.put(kv._1, kv._2); this }
    def -=(key: A): this.type = { underlying remove key; this }

    override def put(k: A, v: B): Option[B] = Option(underlying.put(k, v))

    override def update(k: A, v: B) { underlying.put(k, v) }

    override def remove(k: A): Option[B] = Option(underlying remove k)

    def iterator = enumerationAsScalaIterator(underlying.keys) map (k => (k, underlying get k))

    override def clear() = underlying.clear()
  }

  case class JPropertiesWrapper(underlying: ju.Properties) extends mutable.AbstractMap[String, String]
            with mutable.Map[String, String]
            with mutable.MapLike[String, String, JPropertiesWrapper] {

    override def size = underlying.size

    def get(k: String) = {
      val v = underlying get k
      if (v != null) Some(v.asInstanceOf[String]) else None
    }

    def +=(kv: (String, String)): this.type = { underlying.put(kv._1, kv._2); this }
    def -=(key: String): this.type = { underlying remove key; this }

    override def put(k: String, v: String): Option[String] = {
      val r = underlying.put(k, v)
      if (r != null) Some(r.asInstanceOf[String]) else None
    }

    override def update(k: String, v: String) { underlying.put(k, v) }

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

    override def empty = JPropertiesWrapper(new ju.Properties)

    def getProperty(key: String) = underlying.getProperty(key)

    def getProperty(key: String, defaultValue: String) =
      underlying.getProperty(key, defaultValue)

    def setProperty(key: String, value: String) =
      underlying.setProperty(key, value)
  }
}

@SerialVersionUID(0 - 5857859809262781311L)
object Wrappers extends Wrappers with Serializable
