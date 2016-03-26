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
import scala.language.implicitConversions

import JavaConverters._

/** Defines implicit converter methods from Java to Scala collections. */
trait ToScalaImplicits {

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
  implicit def `iterator asScala`[A](it: ju.Iterator[A]): Iterator[A] = asScalaIterator(it)

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
  implicit def `enumeration AsScalaIterator`[A](i: ju.Enumeration[A]): Iterator[A] = enumerationAsScalaIterator(i)

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
  implicit def `iterable AsScalaIterable`[A](i: jl.Iterable[A]): Iterable[A] = iterableAsScalaIterable(i)

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
  implicit def `collection AsScalaIterable`[A](i: ju.Collection[A]): Iterable[A] = collectionAsScalaIterable(i)

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
  implicit def `list asScalaBuffer`[A](l: ju.List[A]): mutable.Buffer[A] = asScalaBuffer(l)

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
  implicit def `set asScala`[A](s: ju.Set[A]): mutable.Set[A] = asScalaSet(s)

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
   * If the wrapped map is synchronized (e.g. from `java.util.Collections.synchronizedMap`),
   * it is your responsibility to wrap all
   * non-atomic operations with `underlying.synchronized`.
   * This includes `get`, as `java.util.Map`'s API does not allow for an
   * atomic `get` when `null` values may be present.
   *
   * @param m The Map to be converted.
   * @return A Scala mutable Map view of the argument.
   */
  implicit def `map AsScala`[A, B](m: ju.Map[A, B]): mutable.Map[A, B] = mapAsScalaMap(m)

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
  implicit def `map AsScalaConcurrentMap`[A, B](m: juc.ConcurrentMap[A, B]): concurrent.Map[A, B] = mapAsScalaConcurrentMap(m)

  /**
   * Implicitly converts a Java `Dictionary` to a Scala mutable
   * `Map`.
   *
   * The returned Scala `Map` is backed by the provided Java
   * `Dictionary` and any side-effects of using it via the Scala interface
   * will be visible via the Java interface and vice versa.
   *
   * @param p The Dictionary to be converted.
   * @return  A Scala mutable Map view of the argument.
   */
  implicit def `dictionary AsScalaMap`[A, B](p: ju.Dictionary[A, B]): mutable.Map[A, B] = dictionaryAsScalaMap(p)

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
  implicit def `properties AsScalaMap`(p: ju.Properties): mutable.Map[String, String] = propertiesAsScalaMap(p)
}

/** Defines implicit conversions from Scala to Java collections. */
trait ToJavaImplicits {

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
  implicit def `iterator asJava`[A](it: Iterator[A]): ju.Iterator[A] = asJavaIterator(it)

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
  implicit def `enumeration asJava`[A](it: Iterator[A]): ju.Enumeration[A] = asJavaEnumeration(it)

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
  implicit def `iterable asJava`[A](i: Iterable[A]): jl.Iterable[A] = asJavaIterable(i)

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
  implicit def `collection asJava`[A](it: Iterable[A]): ju.Collection[A] = asJavaCollection(it)

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
  implicit def `buffer AsJavaList`[A](b: mutable.Buffer[A]): ju.List[A] = bufferAsJavaList(b)

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
  implicit def `mutableSeq AsJavaList`[A](seq: mutable.Seq[A]): ju.List[A] = mutableSeqAsJavaList(seq)

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
  implicit def `seq AsJavaList`[A](seq: Seq[A]): ju.List[A] = seqAsJavaList(seq)

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
  implicit def `mutableSet AsJavaSet`[A](s: mutable.Set[A]): ju.Set[A] = mutableSetAsJavaSet(s)

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
  implicit def `set AsJavaSet`[A](s: Set[A]): ju.Set[A] = setAsJavaSet(s)

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
  implicit def `mutableMap AsJavaMap`[A, B](m: mutable.Map[A, B]): ju.Map[A, B] = mutableMapAsJavaMap(m)

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
  implicit def `dictionary asJava`[A, B](m: mutable.Map[A, B]): ju.Dictionary[A, B] = asJavaDictionary(m)

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
  implicit def `map AsJavaMap`[A, B](m: Map[A, B]): ju.Map[A, B] = mapAsJavaMap(m)

  /**
   * Implicitly converts a Scala mutable `concurrent.Map` to a Java
   * `ConcurrentMap`.
   *
   * The returned Java `ConcurrentMap` is backed by the provided Scala
   * `concurrent.Map` and any side-effects of using it via the Java interface
   * will be visible via the Scala interface and vice versa.
   *
   * If the Scala `concurrent.Map` was previously obtained from an implicit or
   * explicit call of `mapAsScalaConcurrentMap(java.util.concurrent.ConcurrentMap)`
   * then the original Java ConcurrentMap will be returned.
   *
   * @param m The Scala `concurrent.Map` to be converted.
   * @return A Java `ConcurrentMap` view of the argument.
   */
  implicit def `map AsJavaConcurrentMap`[A, B](m: concurrent.Map[A, B]): juc.ConcurrentMap[A, B] = mapAsJavaConcurrentMap(m)
}

/**
 * Convenience for miscellaneous implicit conversions from Scala to Java collections API.
 *
 * It is recommended to use explicit conversions provided by [[collection.JavaConverters]] instead.
 * Implicit conversions may cause unexpected issues, see [[ImplicitConversions]].
 */
object ImplicitConversionsToJava extends ToJavaImplicits

/**
 * Convenience for miscellaneous implicit conversions from Java to Scala collections API.
 *
 * It is recommended to use explicit conversions provided by [[collection.JavaConverters]] instead.
 * Implicit conversions may cause unexpected issues, see [[ImplicitConversions]].
 */
object ImplicitConversionsToScala extends ToScalaImplicits

/**
 * Convenience for miscellaneous implicit conversions between Java and Scala collections API.
 *
 * It is recommended to use explicit conversions provided by [[collection.JavaConverters]] instead.
 * Implicit conversions may cause unexpected issues. Example:
 *
 * {{{
 *   import collection.convert.ImplicitConversions._
 *   case class StringBox(s: String)
 *   val m = Map(StringBox("one") -> "uno")
 *   m.get("one")
 * }}}
 *
 * The above example returns `null` instead of producing a type error at compile-time. The map is
 * implicitly converted to a `java.util.Map` which provides a method `get(x: AnyRef)`.
 */
object ImplicitConversions extends ToScalaImplicits with ToJavaImplicits
