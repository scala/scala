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
import scala.language.implicitConversions

trait LowPriorityWrapAsScala {
  this: WrapAsScala =>
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
  @deprecated("Use `mapAsScalaConcurrentMap` instead, and use `concurrent.Map` instead of `ConcurrentMap`.", "2.10.0")
  implicit def mapAsScalaDeprecatedConcurrentMap[A, B](m: juc.ConcurrentMap[A, B]): mutable.ConcurrentMap[A, B] =
    asScalaConcurrentMap(m)
}

trait WrapAsScala extends LowPriorityWrapAsScala {
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
   * @param it The `Iterator` to be converted.
   * @return   A Scala `Iterator` view of the argument.
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
  @deprecated("Use `mapAsScalaConcurrentMap` instead, and use `concurrent.Map` instead of `ConcurrentMap`.", "2.10.0")
  def asScalaConcurrentMap[A, B](m: juc.ConcurrentMap[A, B]): mutable.ConcurrentMap[A, B] = m match {
    case cmw: ConcurrentMapDeprecatedWrapper[a, b] => cmw.underlying
    case _                                         => new JConcurrentMapDeprecatedWrapper(m)
  }

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
  implicit def mapAsScalaConcurrentMap[A, B](m: juc.ConcurrentMap[A, B]): concurrent.Map[A, B] = m match {
    case cmw: ConcurrentMapWrapper[a, b]      => cmw.underlying
    case _                                    => new JConcurrentMapWrapper(m)
  }

  /**
   * Implicitly converts a Java `Dictionary` to a Scala mutable
   * `Map[String, String]`.
   *
   * The returned Scala `Map[String, String]` is backed by the provided Java
   * `Dictionary` and any side-effects of using it via the Scala interface
   * will be visible via the Java interface and vice versa.
   *
   * @param p The Dictionary to be converted.
   * @return  A Scala mutable Map[String, String] view of the argument.
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
   * @param p The Properties to be converted.
   * @return  A Scala mutable Map[String, String] view of the argument.
   */
  implicit def propertiesAsScalaMap(p: ju.Properties): mutable.Map[String, String] = p match {
    case _ => new JPropertiesWrapper(p)
  }
}

object WrapAsScala extends WrapAsScala { }
