/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2016, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package convert

import java.{ lang => jl, util => ju }, java.util.{ concurrent => juc }

/** Defines converter methods from Scala to Java collections. */
trait AsJavaConverters {
  import Wrappers._

  /**
   * Converts a Scala `Iterator` to a Java `Iterator`.
   *
   * The returned Java `Iterator` is backed by the provided Scala `Iterator` and any side-effects of
   * using it via the Java interface will be visible via the Scala interface and vice versa.
   *
   * If the Scala `Iterator` was previously obtained from an implicit or explicit call of
   * `[[JavaConverters.asScalaIterator]](java.util.Iterator)` then the original Java `Iterator` will
   * be returned.
   *
   * @param i The Scala `Iterator` to be converted.
   * @return  A Java `Iterator` view of the argument.
   */
  def asJavaIterator[A](i: Iterator[A]): ju.Iterator[A] = i match {
    case null                       => null
    case JIteratorWrapper(wrapped)  => wrapped.asInstanceOf[ju.Iterator[A]]
    case _                          => IteratorWrapper(i)
  }

  /**
   * Converts a Scala `Iterator` to a Java `Enumeration`.
   *
   * The returned Java `Enumeration` is backed by the provided Scala `Iterator` and any side-effects
   * of using it via the Java interface will be visible via the Scala interface and vice versa.
   *
   * If the Scala `Iterator` was previously obtained from an implicit or explicit call of
   * `[[JavaConverters.enumerationAsScalaIterator]](java.util.Enumeration)` then the original Java
   * `Enumeration` will be returned.
   *
   * @param i The Scala `Iterator` to be converted.
   * @return  A Java `Enumeration` view of the argument.
   */
  def asJavaEnumeration[A](i: Iterator[A]): ju.Enumeration[A] = i match {
    case null                         => null
    case JEnumerationWrapper(wrapped) => wrapped.asInstanceOf[ju.Enumeration[A]]
    case _                            => IteratorWrapper(i)
  }

  /**
   * Converts a Scala `Iterable` to a Java `Iterable`.
   *
   * The returned Java `Iterable` is backed by the provided Scala `Iterable` and any side-effects of
   * using it via the Java interface will be visible via the Scala interface and vice versa.
   *
   * If the Scala `Iterable` was previously obtained from an implicit or explicit call of
   * `[[JavaConverters.iterableAsScalaIterable]](java.lang.Iterable)` then the original Java
   * `Iterable` will be returned.
   *
   * @param i The Scala `Iterable` to be converted.
   * @return  A Java `Iterable` view of the argument.
   */
  def asJavaIterable[A](i: Iterable[A]): jl.Iterable[A] = i match {
    case null                       => null
    case JIterableWrapper(wrapped)  => wrapped.asInstanceOf[jl.Iterable[A]]
    case _                          => IterableWrapper(i)
  }

  /**
   * Converts a Scala `Iterable` to an immutable Java `Collection`.
   *
   * If the Scala `Iterable` was previously obtained from an implicit or explicit call of
   * `[[JavaConverters.collectionAsScalaIterable]](java.util.Collection)` then the original Java
   * `Collection` will be returned.
   *
   * @param i The Scala `Iterable` to be converted.
   * @return  A Java `Collection` view of the argument.
   */
  def asJavaCollection[A](i: Iterable[A]): ju.Collection[A] = i match {
    case null                         => null
    case JCollectionWrapper(wrapped)  => wrapped.asInstanceOf[ju.Collection[A]]
    case _                            => new IterableWrapper(i)
  }

  /**
   * Converts a Scala mutable `Buffer` to a Java List.
   *
   * The returned Java List is backed by the provided Scala `Buffer` and any side-effects of using
   * it via the Java interface will be visible via the Scala interface and vice versa.
   *
   * If the Scala `Buffer` was previously obtained from an implicit or explicit call of
   * `[[JavaConverters.asScalaBuffer]](java.util.List)` then the original Java `List` will be
   * returned.
   *
   * @param b The Scala `Buffer` to be converted.
   * @return A Java `List` view of the argument.
   */
  def bufferAsJavaList[A](b: mutable.Buffer[A]): ju.List[A] = b match {
    case null                   => null
    case JListWrapper(wrapped)  => wrapped
    case _                      => new MutableBufferWrapper(b)
  }

  /**
   * Converts a Scala mutable `Seq` to a Java `List`.
   *
   * The returned Java `List` is backed by the provided Scala `Seq` and any side-effects of using it
   * via the Java interface will be visible via the Scala interface and vice versa.
   *
   * If the Scala `Seq` was previously obtained from an implicit or explicit call of
   * `[[JavaConverters.asScalaBuffer]](java.util.List)` then the original Java `List` will be
   * returned.
   *
   * @param s The Scala `Seq` to be converted.
   * @return  A Java `List` view of the argument.
   */
  def mutableSeqAsJavaList[A](s: mutable.Seq[A]): ju.List[A] = s match {
    case null                   => null
    case JListWrapper(wrapped)  => wrapped
    case _                      => new MutableSeqWrapper(s)
  }

  /**
   * Converts a Scala `Seq` to a Java `List`.
   *
   * The returned Java `List` is backed by the provided Scala `Seq` and any side-effects of using it
   * via the Java interface will be visible via the Scala interface and vice versa.
   *
   * If the Scala `Seq` was previously obtained from an implicit or explicit call of
   * `[[JavaConverters.asScalaBuffer]](java.util.List)` then the original Java `List` will be
   * returned.
   *
   * @param s The Scala `Seq` to be converted.
   * @return  A Java `List` view of the argument.
   */
  def seqAsJavaList[A](s: Seq[A]): ju.List[A] = s match {
    case null                   => null
    case JListWrapper(wrapped)  => wrapped.asInstanceOf[ju.List[A]]
    case _                      => new SeqWrapper(s)
  }

  /**
   * Converts a Scala mutable `Set` to a Java `Set`.
   *
   * The returned Java `Set` is backed by the provided Scala `Set` and any side-effects of using it
   * via the Java interface will be visible via the Scala interface and vice versa.
   *
   * If the Scala `Set` was previously obtained from an implicit or explicit call of
   * `[[JavaConverters.asScalaSet]](java.util.Set)` then the original Java `Set` will be returned.
   *
   * @param s The Scala mutable `Set` to be converted.
   * @return  A Java `Set` view of the argument.
   */
  def mutableSetAsJavaSet[A](s: mutable.Set[A]): ju.Set[A] = s match {
    case null                 => null
    case JSetWrapper(wrapped) => wrapped
    case _                    => new MutableSetWrapper(s)
  }

  /**
   * Converts a Scala `Set` to a Java `Set`.
   *
   * The returned Java `Set` is backed by the provided Scala `Set` and any side-effects of using it
   * via the Java interface will be visible via the Scala interface and vice versa.
   *
   * If the Scala `Set` was previously obtained from an implicit or explicit call of
   * `[[JavaConverters.asScalaSet]](java.util.Set)` then the original Java `Set` will be returned.
   *
   * @param s The Scala `Set` to be converted.
   * @return  A Java `Set` view of the argument.
   */
  def setAsJavaSet[A](s: Set[A]): ju.Set[A] = s match {
    case null                 => null
    case JSetWrapper(wrapped) => wrapped
    case _                    => new SetWrapper(s)
  }

  /**
   * Converts a Scala mutable `Map` to a Java `Map`.
   *
   * The returned Java `Map` is backed by the provided Scala `Map` and any side-effects of using it
   * via the Java interface will be visible via the Scala interface and vice versa.
   *
   * If the Scala `Map` was previously obtained from an implicit or explicit call of
   * `[[JavaConverters.mapAsScalaMap]](java.util.Map)` then the original Java `Map` will be
   * returned.
   *
   * @param m The Scala mutable `Map` to be converted.
   * @return  A Java `Map` view of the argument.
   */
  def mutableMapAsJavaMap[A, B](m: mutable.Map[A, B]): ju.Map[A, B] = m match {
    case null                 => null
    case JMapWrapper(wrapped) => wrapped
    case _                    => new MutableMapWrapper(m)
  }

  /**
   * Converts a Scala mutable `Map` to a Java `Dictionary`.
   *
   * The returned Java `Dictionary` is backed by the provided Scala `Dictionary` and any
   * side-effects of using it via the Java interface will be visible via the Scala interface and
   * vice versa.
   *
   * If the Scala `Dictionary` was previously obtained from an implicit or explicit call of
   * `[[JavaConverters.dictionaryAsScalaMap]](java.util.Dictionary)` then the original Java
   * `Dictionary` will be returned.
   *
   * @param m The Scala `Map` to be converted.
   * @return  A Java `Dictionary` view of the argument.
   */
  def asJavaDictionary[A, B](m: mutable.Map[A, B]): ju.Dictionary[A, B] = m match {
    case null                         => null
    case JDictionaryWrapper(wrapped)  => wrapped
    case _                            => new DictionaryWrapper(m)
  }

  /**
   * Converts a Scala `Map` to a Java `Map`.
   *
   * The returned Java `Map` is backed by the provided Scala `Map` and any side-effects of using it
   * via the Java interface will be visible via the Scala interface and vice versa.
   *
   * If the Scala `Map` was previously obtained from an implicit or explicit call of
   * `[[JavaConverters.mapAsScalaMap]](java.util.Map)` then the original Java `Map` will be
   * returned.
   *
   * @param m The Scala `Map` to be converted.
   * @return  A Java `Map` view of the argument.
   */
  def mapAsJavaMap[A, B](m: Map[A, B]): ju.Map[A, B] = m match {
    case null                 => null
    case JMapWrapper(wrapped) => wrapped.asInstanceOf[ju.Map[A, B]]
    case _                    => new MapWrapper(m)
  }

  /**
   * Converts a Scala mutable `concurrent.Map` to a Java `ConcurrentMap`.
   *
   * The returned Java `ConcurrentMap` is backed by the provided Scala `concurrent.Map` and any
   * side-effects of using it via the Java interface will be visible via the Scala interface and
   * vice versa.
   *
   * If the Scala `concurrent.Map` was previously obtained from an implicit or explicit call of
   * `[[JavaConverters.mapAsScalaConcurrentMap]](java.util.concurrent.ConcurrentMap)` then the
   * original Java `ConcurrentMap` will be returned.
   *
   * @param m The Scala `concurrent.Map` to be converted.
   * @return  A Java `ConcurrentMap` view of the argument.
   */
  def mapAsJavaConcurrentMap[A, B](m: concurrent.Map[A, B]): juc.ConcurrentMap[A, B] = m match {
    case null                           => null
    case JConcurrentMapWrapper(wrapped) => wrapped
    case _                              => new ConcurrentMapWrapper(m)
  }
}
