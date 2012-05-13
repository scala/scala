/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package convert

import java.{ lang => jl, util => ju }, java.util.{ concurrent => juc }
import Wrappers._
import language.implicitConversions

trait WrapAsJava {
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
   * @param  it The Iterator to be converted.
   * @return    A Java Iterator view of the argument.
   */
  implicit def asJavaIterator[A](it: Iterator[A]): ju.Iterator[A] = it match {
    case JIteratorWrapper(wrapped) => wrapped.asInstanceOf[ju.Iterator[A]]
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
   * @param it The Iterator to be converted.
   * @return   A Java Enumeration view of the argument.
   */
  implicit def asJavaEnumeration[A](it: Iterator[A]): ju.Enumeration[A] = it match {
    case JEnumerationWrapper(wrapped) => wrapped.asInstanceOf[ju.Enumeration[A]]
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
    case JIterableWrapper(wrapped) => wrapped.asInstanceOf[jl.Iterable[A]]
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
   * @param it The SizedIterable to be converted.
   * @return   A Java Collection view of the argument.
   */
  implicit def asJavaCollection[A](it: Iterable[A]): ju.Collection[A] = it match {
    case JCollectionWrapper(wrapped) => wrapped.asInstanceOf[ju.Collection[A]]
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
   * @param seq The Seq to be converted.
   * @return    A Java List view of the argument.
   */
  implicit def mutableSeqAsJavaList[A](seq: mutable.Seq[A]): ju.List[A] = seq match {
    case JListWrapper(wrapped) => wrapped
    case _ => new MutableSeqWrapper(seq)
  }

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
   * @param seq The Seq to be converted.
   * @return    A Java List view of the argument.
   */
  implicit def seqAsJavaList[A](seq: Seq[A]): ju.List[A] = seq match {
    case JListWrapper(wrapped) => wrapped.asInstanceOf[ju.List[A]]
    case _ => new SeqWrapper(seq)
  }

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
    case JMapWrapper(wrapped) => wrapped.asInstanceOf[ju.Map[A, B]]
    case _ => new MapWrapper(m)
  }

  /**
   * Implicitly converts a Scala mutable `ConcurrentMap` to a Java
   * `ConcurrentMap`.
   *
   * The returned Java `ConcurrentMap` is backed by the provided Scala
   * `ConcurrentMap` and any side-effects of using it via the Java interface
   * will be visible via the Scala interface and vice versa.
   *
   * If the Scala `ConcurrentMap` was previously obtained from an implicit or
   * explicit call of `asScalaConcurrentMap(java.util.concurrect.ConcurrentMap)`
   * then the original Java ConcurrentMap will be returned.
   *
   * @param m The `ConcurrentMap` to be converted.
   * @return A Java `ConcurrentMap` view of the argument.
   */
  @deprecated("Use `concurrent.Map` instead of `ConcurrentMap`.", "2.10.0")
  implicit def asJavaConcurrentMap[A, B](m: mutable.ConcurrentMap[A, B]): juc.ConcurrentMap[A, B] = m match {
    case JConcurrentMapDeprecatedWrapper(wrapped) => wrapped
    case _ => new ConcurrentMapDeprecatedWrapper(m)
  }
  
  /**
   * Implicitly converts a Scala mutable `concurrent.Map` to a Java
   * `ConcurrentMap`.
   *
   * The returned Java `ConcurrentMap` is backed by the provided Scala
   * `concurrent.Map` and any side-effects of using it via the Java interface
   * will be visible via the Scala interface and vice versa.
   *
   * If the Scala `concurrent.Map` was previously obtained from an implicit or
   * explicit call of `mapAsScalaConcurrentMap(java.util.concurrect.ConcurrentMap)`
   * then the original Java ConcurrentMap will be returned.
   *
   * @param m The Scala `concurrent.Map` to be converted.
   * @return A Java `ConcurrentMap` view of the argument.
   */
  implicit def asJavaConcurrentMap[A, B](m: concurrent.Map[A, B]): juc.ConcurrentMap[A, B] = m match {
    case JConcurrentMapWrapper(wrapped) => wrapped
    case _ => new ConcurrentMapWrapper(m)
  }
}

object WrapAsJava extends WrapAsJava { }








