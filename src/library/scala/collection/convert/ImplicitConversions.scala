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
  /** Implicitly converts a Java `Iterator` to a Scala `Iterator`.
   *  @see [[AsScalaConverters.asScalaIterator]]
   */
  implicit def `iterator asScala`[A](it: ju.Iterator[A]): Iterator[A] = asScalaIterator(it)

  /** Implicitly converts a Java `Enumeration` to a Scala `Iterator`.
   *  @see [[AsScalaConverters.enumerationAsScalaIterator]]
   */
  implicit def `enumeration AsScalaIterator`[A](i: ju.Enumeration[A]): Iterator[A] = enumerationAsScalaIterator(i)

  /** Implicitly converts a Java `Iterable` to a Scala `Iterable`.
   *  @see [[AsScalaConverters.iterableAsScalaIterable]]
   */
  implicit def `iterable AsScalaIterable`[A](i: jl.Iterable[A]): Iterable[A] = iterableAsScalaIterable(i)

  /** Implicitly converts a Java `Collection` to an Scala `Iterable`.
   *  @see [[AsScalaConverters.collectionAsScalaIterable]]
   */
  implicit def `collection AsScalaIterable`[A](i: ju.Collection[A]): Iterable[A] = collectionAsScalaIterable(i)

  /** Implicitly converts a Java `List` to a Scala mutable `Buffer`.
   *  @see [[AsScalaConverters.asScalaBuffer]]
   */
  implicit def `list asScalaBuffer`[A](l: ju.List[A]): mutable.Buffer[A] = asScalaBuffer(l)

  /** Implicitly converts a Java `Set` to a Scala mutable `Set`.
   *  @see [[AsScalaConverters.asScalaSet]]
   */
  implicit def `set asScala`[A](s: ju.Set[A]): mutable.Set[A] = asScalaSet(s)

  /** Implicitly converts a Java `Map` to a Scala mutable `Map`.
   *  @see [[AsScalaConverters.mapAsScalaMap]]
   */
  implicit def `map AsScala`[A, B](m: ju.Map[A, B]): mutable.Map[A, B] = mapAsScalaMap(m)

  /** Implicitly converts a Java `ConcurrentMap` to a Scala mutable `ConcurrentMap`.
   *  @see [[AsScalaConverters.mapAsScalaConcurrentMap]]
   */
  implicit def `map AsScalaConcurrentMap`[A, B](m: juc.ConcurrentMap[A, B]): concurrent.Map[A, B] = mapAsScalaConcurrentMap(m)

  /** Implicitly converts a Java `Dictionary` to a Scala mutable `Map`.
   *  @see [[AsScalaConverters.dictionaryAsScalaMap]]
   */
  implicit def `dictionary AsScalaMap`[A, B](p: ju.Dictionary[A, B]): mutable.Map[A, B] = dictionaryAsScalaMap(p)

  /** Implicitly converts a Java `Properties` to a Scala `mutable Map[String, String]`.
   *  @see [[AsScalaConverters.propertiesAsScalaMap]]
   */
  implicit def `properties AsScalaMap`(p: ju.Properties): mutable.Map[String, String] = propertiesAsScalaMap(p)
}

/** Defines implicit conversions from Scala to Java collections. */
trait ToJavaImplicits {
  /** Implicitly converts a Scala `Iterator` to a Java `Iterator`.
   *  @see [[AsJavaConverters.asJavaIterator]]
   */
  implicit def `iterator asJava`[A](it: Iterator[A]): ju.Iterator[A] = asJavaIterator(it)

  /** Implicitly converts a Scala `Iterator` to a Java `Enumeration`.
   *  @see [[AsJavaConverters.asJavaEnumeration]]
   */
  implicit def `enumeration asJava`[A](it: Iterator[A]): ju.Enumeration[A] = asJavaEnumeration(it)

  /** Implicitly converts a Scala `Iterable` to a Java `Iterable`.
   *  @see [[AsJavaConverters.asJavaIterable]]
   */
  implicit def `iterable asJava`[A](i: Iterable[A]): jl.Iterable[A] = asJavaIterable(i)

  /** Implicitly converts a Scala `Iterable` to an immutable Java `Collection`.
   *  @see [[AsJavaConverters.asJavaCollection]]
   */
  implicit def `collection asJava`[A](it: Iterable[A]): ju.Collection[A] = asJavaCollection(it)

  /** Implicitly converts a Scala mutable `Buffer` to a Java `List`.
   *  @see [[AsJavaConverters.bufferAsJavaList]]
   */
  implicit def `buffer AsJavaList`[A](b: mutable.Buffer[A]): ju.List[A] = bufferAsJavaList(b)

  /** Implicitly converts a Scala mutable `Seq` to a Java `List`.
   *  @see [[AsJavaConverters.mutableSeqAsJavaList]]
   */
  implicit def `mutableSeq AsJavaList`[A](seq: mutable.Seq[A]): ju.List[A] = mutableSeqAsJavaList(seq)

  /** Implicitly converts a Scala `Seq` to a Java `List`.
   *  @see [[AsJavaConverters.seqAsJavaList]]
   */
  implicit def `seq AsJavaList`[A](seq: Seq[A]): ju.List[A] = seqAsJavaList(seq)

  /** Implicitly converts a Scala mutable `Set` to a Java `Set`.
   *  @see [[AsJavaConverters.mutableSetAsJavaSet]]
   */
  implicit def `mutableSet AsJavaSet`[A](s: mutable.Set[A]): ju.Set[A] = mutableSetAsJavaSet(s)

  /** Implicitly converts a Scala `Set` to a Java `Set`.
   *  @see [[AsJavaConverters.setAsJavaSet]]
   */
  implicit def `set AsJavaSet`[A](s: Set[A]): ju.Set[A] = setAsJavaSet(s)

  /** Implicitly converts a Scala mutable `Map` to a Java `Map`.
   *  @see [[AsJavaConverters.mutableMapAsJavaMap]]
   */
  implicit def `mutableMap AsJavaMap`[A, B](m: mutable.Map[A, B]): ju.Map[A, B] = mutableMapAsJavaMap(m)

  /** Implicitly converts a Scala mutable `Map` to a Java `Dictionary`.
   *  @see [[AsJavaConverters.asJavaDictionary]]
   */
  implicit def `dictionary asJava`[A, B](m: mutable.Map[A, B]): ju.Dictionary[A, B] = asJavaDictionary(m)

  /** Implicitly converts a Scala `Map` to a Java `Map`.
   *  @see [[AsJavaConverters.mapAsJavaMap]]
   */
  implicit def `map AsJavaMap`[A, B](m: Map[A, B]): ju.Map[A, B] = mapAsJavaMap(m)

  /** Implicitly converts a Scala mutable `concurrent.Map` to a Java `ConcurrentMap`.
   *  @see [[AsJavaConverters.mapAsJavaConcurrentMap]]
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
