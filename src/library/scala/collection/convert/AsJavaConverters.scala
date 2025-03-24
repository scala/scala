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
import java.{lang => jl, util => ju}

import scala.{unchecked => uc}

/** Defines converter methods from Scala to Java collections.
  * These methods are available through the [[scala.jdk.javaapi.CollectionConverters]] object.
  */
trait AsJavaConverters {
  import JavaCollectionWrappers._

  /**
   * Converts a Scala `Iterator` to a Java `Iterator`.
   *
   * The returned Java `Iterator` is backed by the provided Scala `Iterator` and any side-effects of
   * using it via the Java interface will be visible via the Scala interface and vice versa.
   *
   * If the Scala `Iterator` was previously obtained from an implicit or explicit call of
   * `asScala` then the original Java `Iterator` will be returned.
   *
   * @param i The Scala `Iterator` to be converted.
   * @return  A Java `Iterator` view of the argument.
   */
  def asJava[A](i: Iterator[A]): ju.Iterator[A] = i match {
    case null                             => null
    case wrapper: JIteratorWrapper[A @uc] => wrapper.underlying
    case _                                => new IteratorWrapper(i)
  }

  /**
   * Converts a Scala `Iterator` to a Java `Enumeration`.
   *
   * The returned Java `Enumeration` is backed by the provided Scala `Iterator` and any side-effects
   * of using it via the Java interface will be visible via the Scala interface and vice versa.
   *
   * If the Scala `Iterator` was previously obtained from an implicit or explicit call of
   * `asScala` then the original Java `Enumeration` will be returned.
   *
   * @param i The Scala `Iterator` to be converted.
   * @return  A Java `Enumeration` view of the argument.
   */
  def asJavaEnumeration[A](i: Iterator[A]): ju.Enumeration[A] = i match {
    case null                                => null
    case wrapper: JEnumerationWrapper[A @uc] => wrapper.underlying
    case _                                   => new IteratorWrapper(i)
  }

  /**
   * Converts a Scala `Iterable` to a Java `Iterable`.
   *
   * The returned Java `Iterable` is backed by the provided Scala `Iterable` and any side-effects of
   * using it via the Java interface will be visible via the Scala interface and vice versa.
   *
   * If the Scala `Iterable` was previously obtained from an implicit or explicit call of
   * `asScala` then the original Java `Iterable` will be returned.
   *
   * @param i The Scala `Iterable` to be converted.
   * @return  A Java `Iterable` view of the argument.
   */
  def asJava[A](i: Iterable[A]): jl.Iterable[A] = i match {
    case null                             => null
    case wrapper: JIterableWrapper[A @uc] => wrapper.underlying
    case _                                => new IterableWrapper(i)
  }

  /**
   * Converts a Scala `Iterable` to an immutable Java `Collection`.
   *
   * If the Scala `Iterable` was previously obtained from an implicit or explicit call of
   * `asScala` then the original Java `Collection` will be returned.
   *
   * @param i The Scala `Iterable` to be converted.
   * @return  A Java `Collection` view of the argument.
   */
  def asJavaCollection[A](i: Iterable[A]): ju.Collection[A] = i match {
    case null                               => null
    case wrapper: JCollectionWrapper[A @uc] => wrapper.underlying
    case _                                  => new IterableWrapper(i)
  }

  /**
   * Converts a Scala mutable `Buffer` to a Java List.
   *
   * The returned Java List is backed by the provided Scala `Buffer` and any side-effects of using
   * it via the Java interface will be visible via the Scala interface and vice versa.
   *
   * If the Scala `Buffer` was previously obtained from an implicit or explicit call of
   * `asScala` then the original Java `List` will be returned.
   *
   * @param b The Scala `Buffer` to be converted.
   * @return A Java `List` view of the argument.
   */
  def asJava[A](b: mutable.Buffer[A]): ju.List[A] = b match {
    case null                         => null
    case wrapper: JListWrapper[A @uc] => wrapper.underlying
    case _                            => new MutableBufferWrapper(b)
  }

  /**
   * Converts a Scala mutable `Seq` to a Java `List`.
   *
   * The returned Java `List` is backed by the provided Scala `Seq` and any side-effects of using it
   * via the Java interface will be visible via the Scala interface and vice versa.
   *
   * If the Scala `Seq` was previously obtained from an implicit or explicit call of
   * `asScala` then the original Java `List` will be returned.
   *
   * @param s The Scala `Seq` to be converted.
   * @return  A Java `List` view of the argument.
   */
  def asJava[A](s: mutable.Seq[A]): ju.List[A] = s match {
    case null                         => null
    case wrapper: JListWrapper[A @uc] => wrapper.underlying
    case _                            => new MutableSeqWrapper(s)
  }

  /**
   * Converts a Scala `Seq` to a Java `List`.
   *
   * The returned Java `List` is backed by the provided Scala `Seq` and any side-effects of using it
   * via the Java interface will be visible via the Scala interface and vice versa.
   *
   * If the Scala `Seq` was previously obtained from an implicit or explicit call of
   * `asScala` then the original Java `List` will be returned.
   *
   * @param s The Scala `Seq` to be converted.
   * @return  A Java `List` view of the argument.
   */
  def asJava[A](s: Seq[A]): ju.List[A] = s match {
    case null                         => null
    case wrapper: JListWrapper[A @uc] => wrapper.underlying
    case _                            => new SeqWrapper(s)
  }

  /**
   * Converts a Scala mutable `Set` to a Java `Set`.
   *
   * The returned Java `Set` is backed by the provided Scala `Set` and any side-effects of using it
   * via the Java interface will be visible via the Scala interface and vice versa.
   *
   * If the Scala `Set` was previously obtained from an implicit or explicit call of
   * `asScala` then the original Java `Set` will be returned.
   *
   * @param s The Scala mutable `Set` to be converted.
   * @return  A Java `Set` view of the argument.
   */
  def asJava[A](s: mutable.Set[A]): ju.Set[A] = s match {
    case null                        => null
    case wrapper: JSetWrapper[A @uc] => wrapper.underlying
    case _                           => new MutableSetWrapper(s)
  }

    /**
   * Converts a Scala `Set` to a Java `Set`.
   *
   * The returned Java `Set` is backed by the provided Scala `Set` and any side-effects of using it
   * via the Java interface will be visible via the Scala interface and vice versa.
   *
   * If the Scala `Set` was previously obtained from an implicit or explicit call of
   * `asScala` then the original Java `Set` will be returned.
   *
   * @param s The Scala `Set` to be converted.
   * @return  A Java `Set` view of the argument.
   */
  def asJava[A](s: Set[A]): ju.Set[A] = s match {
    case null                        => null
    case wrapper: JSetWrapper[A @uc] => wrapper.underlying
    case _                           => new SetWrapper(s)
  }

  /**
   * Converts a Scala mutable `Map` to a Java `Map`.
   *
   * The returned Java `Map` is backed by the provided Scala `Map` and any side-effects of using it
   * via the Java interface will be visible via the Scala interface and vice versa.
   *
   * If the Scala `Map` was previously obtained from an implicit or explicit call of
   * `asScala` then the original Java `Map` will be returned.
   *
   * @param m The Scala mutable `Map` to be converted.
   * @return  A Java `Map` view of the argument.
   */
  def asJava[K, V](m: mutable.Map[K, V]): ju.Map[K, V] = m match {
    case null                               => null
    case wrapper: JMapWrapper[K @uc, V @uc] => wrapper.underlying
    case _                                  => new MutableMapWrapper(m)
  }

  /**
   * Converts a Scala mutable `Map` to a Java `Dictionary`.
   *
   * The returned Java `Dictionary` is backed by the provided Scala `Dictionary` and any
   * side-effects of using it via the Java interface will be visible via the Scala interface and
   * vice versa.
   *
   * If the Scala `Map` was previously obtained from an implicit or explicit call of
   * `asScala` then the original Java `Dictionary` will be returned.
   *
   * @param m The Scala `Map` to be converted.
   * @return  A Java `Dictionary` view of the argument.
   */
  def asJavaDictionary[K, V](m: mutable.Map[K, V]): ju.Dictionary[K, V] = m match {
    case null                                      => null
    case wrapper: JDictionaryWrapper[K @uc, V @uc] => wrapper.underlying
    case _                                         => new DictionaryWrapper(m)
  }

  /**
   * Converts a Scala `Map` to a Java `Map`.
   *
   * The returned Java `Map` is backed by the provided Scala `Map` and any side-effects of using it
   * via the Java interface will be visible via the Scala interface and vice versa.
   *
   * If the Scala `Map` was previously obtained from an implicit or explicit call of
   * `asScala` then the original Java `Map` will be returned.
   *
   * @param m The Scala `Map` to be converted.
   * @return  A Java `Map` view of the argument.
   */
  def asJava[K, V](m: Map[K, V]): ju.Map[K, V] = m match {
    case null                               => null
    case wrapper: JMapWrapper[K @uc, V @uc] => wrapper.underlying
    case _                                  => new MapWrapper(m)
  }

  /**
   * Converts a Scala mutable `concurrent.Map` to a Java `ConcurrentMap`.
   *
   * The returned Java `ConcurrentMap` is backed by the provided Scala `concurrent.Map` and any
   * side-effects of using it via the Java interface will be visible via the Scala interface and
   * vice versa.
   *
   * If the Scala `concurrent.Map` was previously obtained from an implicit or explicit call of
   * `asScala` then the original Java `ConcurrentMap` will be returned.
   *
   * @param m The Scala `concurrent.Map` to be converted.
   * @return  A Java `ConcurrentMap` view of the argument.
   */
  def asJava[K, V](m: concurrent.Map[K, V]): juc.ConcurrentMap[K, V] = m match {
    case null                                         => null
    case wrapper: JConcurrentMapWrapper[K @uc, V @uc] => wrapper.underlying
    case _                                            => new ConcurrentMapWrapper(m)
  }
}
