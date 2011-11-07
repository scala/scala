/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection

/**   A collection of implicit conversions supporting interoperability between
 *    Scala and Java collections.
 *
 *    The following conversions are supported:
 *{{{
 *    scala.collection.Iterable <=> java.lang.Iterable
 *    scala.collection.Iterable <=> java.util.Collection
 *    scala.collection.Iterator <=> java.util.{ Iterator, Enumeration }
 *    scala.collection.mutable.Buffer <=> java.util.List
 *    scala.collection.mutable.Set <=> java.util.Set
 *    scala.collection.mutable.Map <=> java.util.{ Map, Dictionary }
 *    scala.collection.mutable.ConcurrentMap <=> java.util.concurrent.ConcurrentMap
 *}}}
 *    In all cases, converting from a source type to a target type and back
 *    again will return the original source object, eg.
 *
 *{{{
 *    import scala.collection.JavaConversions._
 *
 *    val sl = new scala.collection.mutable.ListBuffer[Int]
 *    val jl : java.util.List[Int] = sl
 *    val sl2 : scala.collection.mutable.Buffer[Int] = jl
 *    assert(sl eq sl2)
 *}}}
 *  In addition, the following one way conversions are provided:
 *
 *{{{
 *    scala.collection.Seq         => java.util.List
 *    scala.collection.mutable.Seq => java.util.List
 *    scala.collection.Set         => java.util.Set
 *    scala.collection.Map         => java.util.Map
 *    java.util.Properties         => scala.collection.mutable.Map[String, String]
 *}}}
 *
 *  @author Miles Sabin
 *  @author Martin Odersky
 *  @since  2.8
 */
object JavaConversions {
  // Note to implementors: the cavalcade of deprecated methods herein should
  // serve as a warning to any who follow: don't overload implicit methods.

  import java.{ lang => jl, util => ju }
  import java.util.{ concurrent => juc }

  // Scala => Java

  /**
   * Implicitly converts a Scala Iterator to a Java Iterator.
   * The returned Java Iterator is backed by the provided Scala
   * Iterator and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   *
   * If the Scala Iterator was previously obtained from an implicit or
   * explicit call of `asIterator(java.util.Iterator)` then the original
   * Java Iterator will be returned.
   *
   * @param i The Iterator to be converted.
   * @return A Java Iterator view of the argument.
   */
  implicit def asJavaIterator[A](it: Iterator[A]): ju.Iterator[A] = it match {
    case JIteratorWrapper(wrapped) => wrapped
    case _ => IteratorWrapper(it)
  }

  /**
   * Implicitly converts a Scala Iterator to a Java Enumeration.
   * The returned Java Enumeration is backed by the provided Scala
   * Iterator and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   *
   * If the Scala Iterator was previously obtained from an implicit or
   * explicit call of `asIterator(java.util.Enumeration)` then the
   * original Java Enumeration will be returned.
   *
   * @param i The Iterator to be converted.
   * @return A Java Enumeration view of the argument.
   */
  implicit def asJavaEnumeration[A](it: Iterator[A]): ju.Enumeration[A] = it match {
    case JEnumerationWrapper(wrapped) => wrapped
    case _ => IteratorWrapper(it)
  }

  /**
   * Implicitly converts a Scala Iterable to a Java Iterable.
   * The returned Java Iterable is backed by the provided Scala
   * Iterable and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   *
   * If the Scala Iterable was previously obtained from an implicit or
   * explicit call of `asIterable(java.lang.Iterable)` then the original
   * Java Iterable will be returned.
   *
   * @param i The Iterable to be converted.
   * @return A Java Iterable view of the argument.
   */
  implicit def asJavaIterable[A](i: Iterable[A]): jl.Iterable[A] = i match {
    case JIterableWrapper(wrapped) => wrapped
    case _ => IterableWrapper(i)
  }

  /**
   * Implicitly converts a Scala Iterable to an immutable Java
   * Collection.
   *
   * If the Scala Iterable was previously obtained from an implicit or
   * explicit call of `asSizedIterable(java.util.Collection)` then the original
   * Java Collection will be returned.
   *
   * @param i The SizedIterable to be converted.
   * @return A Java Collection view of the argument.
   */
  implicit def asJavaCollection[A](it: Iterable[A]): ju.Collection[A] = it match {
    case JCollectionWrapper(wrapped) => wrapped
    case _ => new IterableWrapper(it)
  }

  /**
   * Implicitly converts a Scala mutable Buffer to a Java List.
   * The returned Java List is backed by the provided Scala
   * Buffer and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   *
   * If the Scala Buffer was previously obtained from an implicit or
   * explicit call of `asBuffer(java.util.List)` then the original
   * Java List will be returned.
   *
   * @param b The Buffer to be converted.
   * @return A Java List view of the argument.
   */
  implicit def bufferAsJavaList[A](b: mutable.Buffer[A]): ju.List[A] = b match {
    case JListWrapper(wrapped) => wrapped
    case _ => new MutableBufferWrapper(b)
  }
  @deprecated("use bufferAsJavaList instead", "2.9.0")
  def asJavaList[A](b : mutable.Buffer[A]): ju.List[A] = bufferAsJavaList[A](b)

  /**
   * Implicitly converts a Scala mutable Seq to a Java List.
   * The returned Java List is backed by the provided Scala
   * Seq and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   *
   * If the Scala Seq was previously obtained from an implicit or
   * explicit call of `asSeq(java.util.List)` then the original
   * Java List will be returned.
   *
   * @param b The Seq to be converted.
   * @return A Java List view of the argument.
   */
  implicit def mutableSeqAsJavaList[A](seq: mutable.Seq[A]): ju.List[A] = seq match {
    case JListWrapper(wrapped) => wrapped
    case _ => new MutableSeqWrapper(seq)
  }
  @deprecated("use mutableSeqAsJavaList instead", "2.9.0")
  def asJavaList[A](b : mutable.Seq[A]): ju.List[A] = mutableSeqAsJavaList[A](b)

  /**
   * Implicitly converts a Scala Seq to a Java List.
   * The returned Java List is backed by the provided Scala
   * Seq and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   *
   * If the Scala Seq was previously obtained from an implicit or
   * explicit call of `asSeq(java.util.List)` then the original
   * Java List will be returned.
   *
   * @param b The Seq to be converted.
   * @return A Java List view of the argument.
   */
  implicit def seqAsJavaList[A](seq: Seq[A]): ju.List[A] = seq match {
    case JListWrapper(wrapped) => wrapped
    case _ => new SeqWrapper(seq)
  }

  @deprecated("use seqAsJavaList instead", "2.9.0")
  def asJavaList[A](b : Seq[A]): ju.List[A] = seqAsJavaList[A](b)

  /**
   * Implicitly converts a Scala mutable Set to a Java Set.
   * The returned Java Set is backed by the provided Scala
   * Set and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   *
   * If the Scala Set was previously obtained from an implicit or
   * explicit call of `asSet(java.util.Set)` then the original
   * Java Set will be returned.
   *
   * @param s The Set to be converted.
   * @return A Java Set view of the argument.
   */
  implicit def mutableSetAsJavaSet[A](s: mutable.Set[A]): ju.Set[A] = s match {
    case JSetWrapper(wrapped) => wrapped
    case _ => new MutableSetWrapper(s)
  }

  @deprecated("use mutableSetAsJavaSet instead", "2.9.0")
  def asJavaSet[A](s : mutable.Set[A]): ju.Set[A] = mutableSetAsJavaSet[A](s)

  /**
   * Implicitly converts a Scala Set to a Java Set.
   * The returned Java Set is backed by the provided Scala
   * Set and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   *
   * If the Scala Set was previously obtained from an implicit or
   * explicit call of asSet(java.util.Set) then the original
   * Java Set will be returned.
   *
   * @param s The Set to be converted.
   * @return A Java Set view of the argument.
   */
  implicit def setAsJavaSet[A](s: Set[A]): ju.Set[A] = s match {
    case JSetWrapper(wrapped) => wrapped
    case _ => new SetWrapper(s)
  }

  @deprecated("use setAsJavaSet instead", "2.9.0")
  def asJavaSet[A](s: Set[A]): ju.Set[A] = setAsJavaSet[A](s)

  /**
   * Implicitly converts a Scala mutable Map to a Java Map.
   * The returned Java Map is backed by the provided Scala
   * Map and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   *
   * If the Scala Map was previously obtained from an implicit or
   * explicit call of `asMap(java.util.Map)` then the original
   * Java Map will be returned.
   *
   * @param m The Map to be converted.
   * @return A Java Map view of the argument.
   */
  implicit def mutableMapAsJavaMap[A, B](m: mutable.Map[A, B]): ju.Map[A, B] = m match {
    //case JConcurrentMapWrapper(wrapped) => wrapped
    case JMapWrapper(wrapped) => wrapped
    case _ => new MutableMapWrapper(m)
  }

  @deprecated("use mutableMapAsJavaMap instead", "2.9.0")
  def asJavaMap[A, B](m : mutable.Map[A, B]): ju.Map[A, B] = mutableMapAsJavaMap[A, B](m)

  /**
   * Implicitly converts a Scala mutable `Map` to a Java `Dictionary`.
   *
   * The returned Java `Dictionary` is backed by the provided Scala
   * `Dictionary` and any side-effects of using it via the Java interface
   * will be visible via the Scala interface and vice versa.
   *
   * If the Scala `Dictionary` was previously obtained from an implicit or
   * explicit call of `asMap(java.util.Dictionary)` then the original
   * Java Dictionary will be returned.
   *
   * @param m The `Map` to be converted.
   * @return A Java `Dictionary` view of the argument.
   */
  implicit def asJavaDictionary[A, B](m: mutable.Map[A, B]): ju.Dictionary[A, B] = m match {
    //case JConcurrentMapWrapper(wrapped) => wrapped
    case JDictionaryWrapper(wrapped) => wrapped
    case _ => new DictionaryWrapper(m)
  }

  /**
   * Implicitly converts a Scala `Map` to a Java `Map`.
   *
   * The returned Java `Map` is backed by the provided Scala `Map` and
   * any side-effects of using it via the Java interface will be visible
   * via the Scala interface and vice versa.
   *
   * If the Scala `Map` was previously obtained from an implicit or
   * explicit call of `asMap(java.util.Map)` then the original
   * Java `Map` will be returned.
   *
   * @param m The `Map` to be converted.
   * @return A Java `Map` view of the argument.
   */
  implicit def mapAsJavaMap[A, B](m: Map[A, B]): ju.Map[A, B] = m match {
    //case JConcurrentMapWrapper(wrapped) => wrapped
    case JMapWrapper(wrapped) => wrapped
    case _ => new MapWrapper(m)
  }

  @deprecated("use mapAsJavaMap instead", "2.9.0")
  def asJavaMap[A, B](m : Map[A, B]): ju.Map[A, B] = mapAsJavaMap[A, B](m)

  /**
   * Implicitly converts a Scala mutable `ConcurrentMap` to a Java
   * `ConcurrentMap`.
   *
   * The returned Java `ConcurrentMap` is backed by the provided Scala
   * `ConcurrentMap` and any side-effects of using it via the Java interface
   * will be visible via the Scala interface and vice versa.
   *
   * If the Scala `ConcurrentMap` was previously obtained from an implicit or
   * explicit call of `asConcurrentMap(java.util.concurrect.ConcurrentMap)`
   * then the original Java ConcurrentMap will be returned.
   *
   * @param m The `ConcurrentMap` to be converted.
   * @return A Java `ConcurrentMap` view of the argument.
   */
  implicit def asJavaConcurrentMap[A, B](m: mutable.ConcurrentMap[A, B]): juc.ConcurrentMap[A, B] = m match {
    case JConcurrentMapWrapper(wrapped) => wrapped
    case _ => new ConcurrentMapWrapper(m)
  }

  // Java => Scala

  /**
   * Implicitly converts a Java `Iterator` to a Scala `Iterator`.
   *
   * The returned Scala `Iterator` is backed by the provided Java `Iterator`
   * and any side-effects of using it via the Scala interface will be visible
   * via the Java interface and vice versa.
   *
   * If the Java `Iterator` was previously obtained from an implicit or
   * explicit call of `asIterator(scala.collection.Iterator)` then the
   * original Scala `Iterator` will be returned.
   *
   * @param i The `Iterator` to be converted.
   * @return A Scala `Iterator` view of the argument.
   */
  implicit def asScalaIterator[A](it: ju.Iterator[A]): Iterator[A] = it match {
    case IteratorWrapper(wrapped) => wrapped
    case _ => JIteratorWrapper(it)
  }

  /**
   * Implicitly converts a Java Enumeration to a Scala Iterator.
   * The returned Scala Iterator is backed by the provided Java
   * Enumeration and any side-effects of using it via the Scala interface will
   * be visible via the Java interface and vice versa.
   *
   * If the Java Enumeration was previously obtained from an implicit or
   * explicit call of `enumerationAsScalaIterator(scala.collection.Iterator)`
   * then the original Scala Iterator will be returned.
   *
   * @param i The Enumeration to be converted.
   * @return A Scala Iterator view of the argument.
   */
  implicit def enumerationAsScalaIterator[A](i: ju.Enumeration[A]): Iterator[A] = i match {
    case IteratorWrapper(wrapped) => wrapped
    case _ => JEnumerationWrapper(i)
  }

  /**
   * Implicitly converts a Java `Iterable` to a Scala `Iterable`.
   *
   * The returned Scala `Iterable` is backed by the provided Java `Iterable`
   * and any side-effects of using it via the Scala interface will be visible
   * via the Java interface and vice versa.
   *
   * If the Java `Iterable` was previously obtained from an implicit or
   * explicit call of `iterableAsScalaIterable(scala.collection.Iterable)`
   * then the original Scala Iterable will be returned.
   *
   * @param i The Iterable to be converted.
   * @return A Scala Iterable view of the argument.
   */
  implicit def iterableAsScalaIterable[A](i: jl.Iterable[A]): Iterable[A] = i match {
    case IterableWrapper(wrapped) => wrapped
    case _ => JIterableWrapper(i)
  }

  @deprecated("use iterableAsScalaIterable instead", "2.9.0")
  def asScalaIterable[A](i : jl.Iterable[A]): Iterable[A] = iterableAsScalaIterable[A](i)

  /**
   * Implicitly converts a Java `Collection` to an Scala `Iterable`.
   *
   * If the Java `Collection` was previously obtained from an implicit or
   * explicit call of `collectionAsScalaIterable(scala.collection.SizedIterable)`
   * then the original Scala `Iterable` will be returned.
   *
   * @param i The Collection to be converted.
   * @return A Scala Iterable view of the argument.
   */
  implicit def collectionAsScalaIterable[A](i: ju.Collection[A]): Iterable[A] = i match {
    case IterableWrapper(wrapped) => wrapped
    case _ => JCollectionWrapper(i)
  }
  @deprecated("use collectionAsScalaIterable instead", "2.9.0")
  def asScalaIterable[A](i : ju.Collection[A]): Iterable[A] = collectionAsScalaIterable[A](i)

  /**
   * Implicitly converts a Java `List` to a Scala mutable `Buffer`.
   *
   * The returned Scala `Buffer` is backed by the provided Java `List`
   * and any side-effects of using it via the Scala interface will
   * be visible via the Java interface and vice versa.
   *
   * If the Java `List` was previously obtained from an implicit or
   * explicit call of `asScalaBuffer(scala.collection.mutable.Buffer)`
   * then the original Scala `Buffer` will be returned.
   *
   * @param l The `List` to be converted.
   * @return A Scala mutable `Buffer` view of the argument.
   */
  implicit def asScalaBuffer[A](l: ju.List[A]): mutable.Buffer[A] = l match {
    case MutableBufferWrapper(wrapped) => wrapped
    case _ =>new JListWrapper(l)
  }

  /**
   * Implicitly converts a Java Set to a Scala mutable Set.
   * The returned Scala Set is backed by the provided Java
   * Set and any side-effects of using it via the Scala interface will
   * be visible via the Java interface and vice versa.
   *
   * If the Java Set was previously obtained from an implicit or
   * explicit call of `asScalaSet(scala.collection.mutable.Set)` then
   * the original Scala Set will be returned.
   *
   * @param s The Set to be converted.
   * @return A Scala mutable Set view of the argument.
   */
  implicit def asScalaSet[A](s: ju.Set[A]): mutable.Set[A] = s match {
    case MutableSetWrapper(wrapped) => wrapped
    case _ =>new JSetWrapper(s)
  }

  /**
   * Implicitly converts a Java `Map` to a Scala mutable `Map`.
   *
   * The returned Scala `Map` is backed by the provided Java `Map` and any
   * side-effects of using it via the Scala interface will be visible via
   * the Java interface and vice versa.
   *
   * If the Java `Map` was previously obtained from an implicit or
   * explicit call of `mapAsScalaMap(scala.collection.mutable.Map)` then
   * the original Scala Map will be returned.
   *
   * @param m The Map to be converted.
   * @return A Scala mutable Map view of the argument.
   */
  implicit def mapAsScalaMap[A, B](m: ju.Map[A, B]): mutable.Map[A, B] = m match {
    //case ConcurrentMapWrapper(wrapped) => wrapped
    case MutableMapWrapper(wrapped) => wrapped
    case _ => new JMapWrapper(m)
  }

  @deprecated("use mapAsScalaMap instead", "2.9.0")
  def asScalaMap[A, B](m: ju.Map[A, B]): mutable.Map[A, B] = mapAsScalaMap[A, B](m)

  /**
   * Implicitly converts a Java ConcurrentMap to a Scala mutable ConcurrentMap.
   * The returned Scala ConcurrentMap is backed by the provided Java
   * ConcurrentMap and any side-effects of using it via the Scala interface will
   * be visible via the Java interface and vice versa.
   *
   * If the Java ConcurrentMap was previously obtained from an implicit or
   * explicit call of `asConcurrentMap(scala.collection.mutable.ConcurrentMap)`
   * then the original Scala ConcurrentMap will be returned.
   *
   * @param m The ConcurrentMap to be converted.
   * @return A Scala mutable ConcurrentMap view of the argument.
   */
  implicit def asScalaConcurrentMap[A, B](m: juc.ConcurrentMap[A, B]): mutable.ConcurrentMap[A, B] = m match {
    case cmw: ConcurrentMapWrapper[a, b] => cmw.underlying
    case _                               => new JConcurrentMapWrapper(m)
  }

  /**
   * Implicitly converts a Java `Dictionary` to a Scala mutable
   * `Map[String, String]`.
   *
   * The returned Scala `Map[String, String]` is backed by the provided Java
   * `Dictionary` and any side-effects of using it via the Scala interface
   * will be visible via the Java interface and vice versa.
   *
   * @param m The Dictionary to be converted.
   * @return A Scala mutable Map[String, String] view of the argument.
   */
  implicit def dictionaryAsScalaMap[A, B](p: ju.Dictionary[A, B]): mutable.Map[A, B] = p match {
    case DictionaryWrapper(wrapped) => wrapped
    case _ => new JDictionaryWrapper(p)
  }

  /**
   * Implicitly converts a Java `Properties` to a Scala `mutable Map[String, String]`.
   *
   * The returned Scala `Map[String, String]` is backed by the provided Java
   * `Properties` and any side-effects of using it via the Scala interface
   * will be visible via the Java interface and vice versa.
   *
   * @param m The Properties to be converted.
   * @return A Scala mutable Map[String, String] view of the argument.
   */
  implicit def propertiesAsScalaMap(p: ju.Properties): mutable.Map[String, String] = p match {
    case _ => new JPropertiesWrapper(p)
  }

  @deprecated("use propertiesAsScalaMap instead", "2.9.0")
  def asScalaMap(p: ju.Properties): mutable.Map[String, String] = propertiesAsScalaMap(p)

  // Private implementations (shared by JavaConverters) ...

  trait IterableWrapperTrait[A] extends ju.AbstractCollection[A] {
    val underlying: Iterable[A]
    def size = underlying.size
    override def iterator = IteratorWrapper(underlying.iterator)
    override def isEmpty = underlying.isEmpty
  }

  case class IteratorWrapper[A](underlying: Iterator[A])
  extends ju.Iterator[A] with ju.Enumeration[A] {
    def hasNext = underlying.hasNext
    def next() = underlying.next
    def hasMoreElements = underlying.hasNext
    def nextElement() = underlying.next
    def remove() = throw new UnsupportedOperationException
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

  case class IterableWrapper[A](underlying: Iterable[A])
                extends ju.AbstractCollection[A]
                   with IterableWrapperTrait[A] { }

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

  case class MutableSeqWrapper[A](underlying: mutable.Seq[A])
  extends ju.AbstractList[A] with IterableWrapperTrait[A] {
    def get(i: Int) = underlying(i)
    override def set(i: Int, elem: A) = {
      val p = underlying(i)
      underlying(i) = elem
      p
    }
  }

  case class MutableBufferWrapper[A](underlying: mutable.Buffer[A])
  extends ju.AbstractList[A] with IterableWrapperTrait[A] {
    def get(i: Int) = underlying(i)
    override def set(i: Int, elem: A) = { val p = underlying(i); underlying(i) = elem; p }
    override def add(elem: A) = { underlying append elem; true }
    override def remove(i: Int) = underlying remove i
  }

  case class JListWrapper[A](val underlying: ju.List[A]) extends mutable.AbstractBuffer[A] with mutable.Buffer[A] {
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
  }

  class SetWrapper[A](underlying: Set[A]) extends ju.AbstractSet[A] {
    self =>
    def size = underlying.size
    def iterator = new ju.Iterator[A] {
      val ui = underlying.iterator
      var prev: Option[A] = None
      def hasNext = ui.hasNext
      def next = { val e = ui.next; prev = Some(e); e }
      def remove = prev match {
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

  case class JSetWrapper[A](underlying: ju.Set[A])
  extends mutable.AbstractSet[A]
     with mutable.Set[A]
     with mutable.SetLike[A, JSetWrapper[A]] {

    override def size = underlying.size

    def iterator = underlying.iterator

    def contains(elem: A): Boolean = underlying.contains(elem)

    def +=(elem: A): this.type = { underlying add elem; this }
    def -=(elem: A): this.type = { underlying remove elem; this }

    override def add(elem: A): Boolean = underlying add elem
    override def remove(elem: A): Boolean = underlying remove elem
    override def clear() = underlying.clear()

    override def empty = JSetWrapper(new ju.HashSet[A])
  }

  class MapWrapper[A, B](underlying: Map[A, B]) extends ju.AbstractMap[A, B] { self =>
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
          val (k, v) = ui.next
          prev = Some(k)
          new ju.Map.Entry[A, B] {
            def getKey = k
            def getValue = v
            def setValue(v1 : B) = self.put(k, v1)
            override def hashCode = k.hashCode + v.hashCode
            override def equals(other: Any) = other match {
              case e: ju.Map.Entry[_, _] => k == e.getKey && v == e.getValue
              case _ => false
            }
          }
        }

        def remove() {
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
  }

  case class MutableMapWrapper[A, B](underlying: mutable.Map[A, B])
  extends MapWrapper[A, B](underlying) {
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

  trait JMapWrapperLike[A, B, +Repr <: mutable.MapLike[A, B, Repr] with mutable.Map[A, B]]
  extends mutable.Map[A, B] with mutable.MapLike[A, B, Repr] {
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

    override def put(k: A, v: B): Option[B] = {
      val r = underlying.put(k, v)
      if (r != null) Some(r) else None
    }

    override def update(k: A, v: B) { underlying.put(k, v) }

    override def remove(k: A): Option[B] = {
      val r = underlying remove k
      if (r != null) Some(r) else None
    }

    def iterator: Iterator[(A, B)] = new AbstractIterator[(A, B)] {
      val ui = underlying.entrySet.iterator
      def hasNext = ui.hasNext
      def next() = { val e = ui.next(); (e.getKey, e.getValue) }
    }

    override def clear() = underlying.clear()

    override def empty: Repr = null.asInstanceOf[Repr]
  }

  case class JMapWrapper[A, B](val underlying : ju.Map[A, B])
  extends mutable.AbstractMap[A, B] with JMapWrapperLike[A, B, JMapWrapper[A, B]] {
    override def empty = JMapWrapper(new ju.HashMap[A, B])
  }

  class ConcurrentMapWrapper[A, B](override val underlying: mutable.ConcurrentMap[A, B])
  extends MutableMapWrapper[A, B](underlying) with juc.ConcurrentMap[A, B] {

    def putIfAbsent(k: A, v: B) = underlying.putIfAbsent(k, v) match {
      case Some(v) => v
      case None => null.asInstanceOf[B]
    }

    def remove(k: AnyRef, v: AnyRef) = try {
      underlying.remove(k.asInstanceOf[A], v.asInstanceOf[B])
    } catch {
      case ex: ClassCastException =>
        false
    }

    def replace(k: A, v: B): B = underlying.replace(k, v) match {
      case Some(v) => v
      case None => null.asInstanceOf[B]
    }

    def replace(k: A, oldval: B, newval: B) = underlying.replace(k, oldval, newval)
  }

  case class JConcurrentMapWrapper[A, B](val underlying: juc.ConcurrentMap[A, B])
  extends mutable.AbstractMap[A, B] with JMapWrapperLike[A, B, JConcurrentMapWrapper[A, B]] with mutable.ConcurrentMap[A, B] {
    override def get(k: A) = {
      val v = underlying get k
      if (v != null) Some(v)
      else None
    }

    override def empty = new JConcurrentMapWrapper(new juc.ConcurrentHashMap[A, B])

    def putIfAbsent(k: A, v: B): Option[B] = {
      val r = underlying.putIfAbsent(k, v)
      if (r != null) Some(r) else None
    }

    def remove(k: A, v: B): Boolean = underlying.remove(k, v)

    def replace(k: A, v: B): Option[B] = {
      val prev = underlying.replace(k, v)
      if (prev != null) Some(prev) else None
    }

    def replace(k: A, oldvalue: B, newvalue: B): Boolean =
      underlying.replace(k, oldvalue, newvalue)
  }

  case class DictionaryWrapper[A, B](underlying: mutable.Map[A, B])
  extends ju.Dictionary[A, B] {
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

  case class JDictionaryWrapper[A, B](underlying: ju.Dictionary[A, B])
  extends mutable.AbstractMap[A, B] with mutable.Map[A, B] {
    override def size: Int = underlying.size

    def get(k: A) = {
      val v = underlying get k
      if (v != null) Some(v) else None
    }

    def +=(kv: (A, B)): this.type = { underlying.put(kv._1, kv._2); this }
    def -=(key: A): this.type = { underlying remove key; this }

    override def put(k: A, v: B): Option[B] = {
      val r = underlying.put(k, v)
      if (r != null) Some(r) else None
    }

    override def update(k: A, v: B) { underlying.put(k, v) }

    override def remove(k: A): Option[B] = {
      val r = underlying remove k
      if (r != null) Some(r) else None
    }

    def iterator = enumerationAsScalaIterator(underlying.keys) map (k => (k, underlying get k))

    override def clear() = underlying.clear()
  }

  case class JPropertiesWrapper(underlying: ju.Properties)
  extends mutable.AbstractMap[String, String]
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
