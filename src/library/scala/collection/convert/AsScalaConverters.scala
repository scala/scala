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

/** Defines converter methods from Java to Scala collections. */
trait AsScalaConverters {
  import Wrappers._

  /**
   * Converts a Java `Iterator` to a Scala `Iterator`.
   *
   * The returned Scala `Iterator` is backed by the provided Java `Iterator` and any side-effects of
   * using it via the Scala interface will be visible via the Java interface and vice versa.
   *
   * If the Java `Iterator` was previously obtained from an implicit or explicit call of
   * `[[JavaConverters.asJavaIterator]](scala.collection.Iterator)` then the original Scala
   * `Iterator` will be returned.
   *
   * @param i The Java `Iterator` to be converted.
   * @return  A Scala `Iterator` view of the argument.
   */
  def asScalaIterator[A](i: ju.Iterator[A]): Iterator[A] = i match {
    case null                     => null
    case IteratorWrapper(wrapped) => wrapped
    case _                        => JIteratorWrapper(i)
  }

  /**
   * Converts a Java `Enumeration` to a Scala `Iterator`.
   *
   * The returned Scala `Iterator` is backed by the provided Java `Enumeration` and any side-effects
   * of using it via the Scala interface will be visible via the Java interface and vice versa.
   *
   * If the Java `Enumeration` was previously obtained from an implicit or explicit call of
   * `[[JavaConverters.asJavaEnumeration]](scala.collection.Iterator)` then the original Scala
   * `Iterator` will be returned.
   *
   * @param i The Java `Enumeration` to be converted.
   * @return  A Scala `Iterator` view of the argument.
   */
  def enumerationAsScalaIterator[A](i: ju.Enumeration[A]): Iterator[A] = i match {
    case null                     => null
    case IteratorWrapper(wrapped) => wrapped
    case _                        => JEnumerationWrapper(i)
  }

  /**
   * Converts a Java `Iterable` to a Scala `Iterable`.
   *
   * The returned Scala `Iterable` is backed by the provided Java `Iterable` and any side-effects of
   * using it via the Scala interface will be visible via the Java interface and vice versa.
   *
   * If the Java `Iterable` was previously obtained from an implicit or explicit call of
   * `[[JavaConverters.asJavaIterable]](scala.collection.Iterable) then the original Scala
   * `Iterable` will be returned.
   *
   * @param i The Java `Iterable` to be converted.
   * @return  A Scala `Iterable` view of the argument.
   */
  def iterableAsScalaIterable[A](i: jl.Iterable[A]): Iterable[A] = i match {
    case null                     => null
    case IterableWrapper(wrapped) => wrapped
    case _                        => JIterableWrapper(i)
  }

  /**
   * Converts a Java `Collection` to an Scala `Iterable`.
   *
   * If the Java `Collection` was previously obtained from an implicit or explicit call of
   * `[[JavaConverters.asJavaCollection]](scala.collection.Iterable)` then the original Scala
   * `Iterable` will be returned.
   *
   * @param i The Java `Collection` to be converted.
   * @return  A Scala `Iterable` view of the argument.
   */
  def collectionAsScalaIterable[A](i: ju.Collection[A]): Iterable[A] = i match {
    case null                     => null
    case IterableWrapper(wrapped) => wrapped
    case _                        => JCollectionWrapper(i)
  }

  /**
   * Converts a Java `List` to a Scala mutable `Buffer`.
   *
   * The returned Scala `Buffer` is backed by the provided Java `List` and any side-effects of using
   * it via the Scala interface will be visible via the Java interface and vice versa.
   *
   * If the Java `List` was previously obtained from an implicit or explicit call of
   * `[[JavaConverters.bufferAsJavaList]](scala.collection.mutable.Buffer)` then the original Scala
   * `Buffer` will be returned.
   *
   * @param l The Java `List` to be converted.
   * @return A Scala mutable `Buffer` view of the argument.
   */
  def asScalaBuffer[A](l: ju.List[A]): mutable.Buffer[A] = l match {
    case null                           => null
    case MutableBufferWrapper(wrapped)  => wrapped
    case _                              => new JListWrapper(l)
  }

  /**
   * Converts a Java `Set` to a Scala mutable `Set`.
   *
   * The returned Scala `Set` is backed by the provided Java `Set` and any side-effects of using it
   * via the Scala interface will be visible via the Java interface and vice versa.
   *
   * If the Java `Set` was previously obtained from an implicit or explicit call of
   * `[[JavaConverters.mutableSetAsJavaSet]](scala.collection.mutable.Set)` then the original Scala
   * `Set` will be returned.
   *
   * @param s The Java `Set` to be converted.
   * @return  A Scala mutable `Set` view of the argument.
   */
  def asScalaSet[A](s: ju.Set[A]): mutable.Set[A] = s match {
    case null                       => null
    case MutableSetWrapper(wrapped) => wrapped
    case _                          => new JSetWrapper(s)
  }

  /**
   * Converts a Java `Map` to a Scala mutable `Map`.
   *
   * The returned Scala `Map` is backed by the provided Java `Map` and any side-effects of using it
   * via the Scala interface will be visible via the Java interface and vice versa.
   *
   * If the Java `Map` was previously obtained from an implicit or explicit call of
   * `[[JavaConverters.mutableMapAsJavaMap]](scala.collection.mutable.Map)` then the original Scala
   * `Map` will be returned.
   *
   * If the wrapped map is synchronized (e.g. from `java.util.Collections.synchronizedMap`), it is
   * your responsibility to wrap all non-atomic operations with `underlying.synchronized`.
   * This includes `get`, as `java.util.Map`'s API does not allow for an atomic `get` when `null`
   * values may be present.
   *
   * @param m The Java `Map` to be converted.
   * @return  A Scala mutable `Map` view of the argument.
   */
  def mapAsScalaMap[A, B](m: ju.Map[A, B]): mutable.Map[A, B] = m match {
    case null                       => null
    case MutableMapWrapper(wrapped) => wrapped
    case _                          => new JMapWrapper(m)
  }

  /**
   * Converts a Java `ConcurrentMap` to a Scala mutable `ConcurrentMap`.
   *
   * The returned Scala `ConcurrentMap` is backed by the provided Java `ConcurrentMap` and any
   * side-effects of using it via the Scala interface will be visible via the Java interface and
   * vice versa.
   *
   * If the Java `ConcurrentMap` was previously obtained from an implicit or explicit call of
   * `[[JavaConverters.mapAsJavaConcurrentMap]](scala.collection.mutable.ConcurrentMap)`
   * then the original Scala `ConcurrentMap` will be returned.
   *
   * @param m The Java `ConcurrentMap` to be converted.
   * @return  A Scala mutable `ConcurrentMap` view of the argument.
   */
  def mapAsScalaConcurrentMap[A, B](m: juc.ConcurrentMap[A, B]): concurrent.Map[A, B] = m match {
    case null                             => null
    case cmw: ConcurrentMapWrapper[_, _]  => cmw.underlying
    case _                                => new JConcurrentMapWrapper(m)
  }

  /**
   * Converts a Java `Dictionary` to a Scala mutable `Map`.
   *
   * The returned Scala `Map` is backed by the provided Java `Dictionary` and any side-effects of
   * using it via the Scala interface will be visible via the Java interface and vice versa.
   *
   * If the Java `Dictionary` was previously obtained from an implicit or explicit call of
   * `[[JavaConverters.asJavaDictionary]](scala.collection.mutable.Map)` then the original
   * Scala `Map` will be returned.
   *
   * @param p The Java `Dictionary` to be converted.
   * @return  A Scala mutable `Map` view of the argument.
   */
  def dictionaryAsScalaMap[A, B](p: ju.Dictionary[A, B]): mutable.Map[A, B] = p match {
    case null                       => null
    case DictionaryWrapper(wrapped) => wrapped
    case _                          => new JDictionaryWrapper(p)
  }

  /**
   * Converts a Java `Properties` to a Scala mutable `Map[String, String]`.
   *
   * The returned Scala `Map[String, String]` is backed by the provided Java `Properties` and any
   * side-effects of using it via the Scala interface will be visible via the Java interface and
   * vice versa.
   *
   * @param p The Java `Properties` to be converted.
   * @return  A Scala mutable `Map[String, String]` view of the argument.
   */
  def propertiesAsScalaMap(p: ju.Properties): mutable.Map[String, String] = p match {
    case null => null
    case _    => new JPropertiesWrapper(p)
  }
}
