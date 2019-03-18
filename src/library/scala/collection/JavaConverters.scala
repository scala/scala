/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.collection

import java.util.{concurrent => juc}
import java.{lang => jl, util => ju}

import scala.collection.convert._

/** A variety of decorators that enable converting between
 *  Scala and Java collections using extension methods, `asScala` and `asJava`.
 *
 *  The extension methods return adapters for the corresponding API.
 *
 *  The following conversions are supported via `asScala` and `asJava`:
 *{{{
 *    scala.collection.Iterable       <=> java.lang.Iterable
 *    scala.collection.Iterator       <=> java.util.Iterator
 *    scala.collection.mutable.Buffer <=> java.util.List
 *    scala.collection.mutable.Set    <=> java.util.Set
 *    scala.collection.mutable.Map    <=> java.util.Map
 *    scala.collection.concurrent.Map <=> java.util.concurrent.ConcurrentMap
 *}}}
 *  The following conversions are supported via `asScala` and through
 *  specially-named extension methods to convert to Java collections, as shown:
 *{{{
 *    scala.collection.Iterable    <=> java.util.Collection   (via asJavaCollection)
 *    scala.collection.Iterator    <=> java.util.Enumeration  (via asJavaEnumeration)
 *    scala.collection.mutable.Map <=> java.util.Dictionary   (via asJavaDictionary)
 *}}}
 *  In addition, the following one-way conversions are provided via `asJava`:
 *{{{
 *    scala.collection.Seq         => java.util.List
 *    scala.collection.mutable.Seq => java.util.List
 *    scala.collection.Set         => java.util.Set
 *    scala.collection.Map         => java.util.Map
 *}}}
 *  The following one way conversion is provided via `asScala`:
 *{{{
 *    java.util.Properties => scala.collection.mutable.Map
 *}}}
 *  In all cases, converting from a source type to a target type and back
 *  again will return the original source object. For example:
 *  {{{
 *    import scala.collection.JavaConverters._
 *
 *    val source = new scala.collection.mutable.ListBuffer[Int]
 *    val target: java.util.List[Int] = source.asJava
 *    val other: scala.collection.mutable.Buffer[Int] = target.asScala
 *    assert(source eq other)
 *  }}}
 *  Alternatively, the conversion methods have descriptive names and can be invoked explicitly.
 *  {{{
 *    scala> val vs = java.util.Arrays.asList("hi", "bye")
 *    vs: java.util.List[String] = [hi, bye]
 *
 *    scala> val ss = asScalaIterator(vs.iterator)
 *    ss: Iterator[String] = <iterator>
 *
 *    scala> .toList
 *    res0: List[String] = List(hi, bye)
 *
 *    scala> val ss = asScalaBuffer(vs)
 *    ss: scala.collection.mutable.Buffer[String] = Buffer(hi, bye)
 *  }}}
 *
 *  @since  2.8.1
 */
@deprecated("Use `scala.jdk.CollectionConverters` instead", "2.13.0")
object JavaConverters extends AsJavaConverters with AsJavaExtensions with AsScalaConverters with AsScalaExtensions {
  @deprecated("Use `asJava` instead", "2.13.0")
  def asJavaIterator[A](i: Iterator[A]): ju.Iterator[A] = asJava(i)

  @deprecated("Use `asJava` instead", "2.13.0")
  def asJavaIterable[A](i: Iterable[A]): jl.Iterable[A] = asJava(i)

  @deprecated("Use `asJava` instead", "2.13.0")
  def bufferAsJavaList[A](b: mutable.Buffer[A]): ju.List[A] = asJava(b)

  @deprecated("Use `asJava` instead", "2.13.0")
  def mutableSeqAsJavaList[A](s: mutable.Seq[A]): ju.List[A] = asJava(s)

  @deprecated("Use `asJava` instead", "2.13.0")
  def seqAsJavaList[A](s: Seq[A]): ju.List[A] = asJava(s)

  @deprecated("Use `asJava` instead", "2.13.0")
  def mutableSetAsJavaSet[A](s: mutable.Set[A]): ju.Set[A] = asJava(s)

  @deprecated("Use `asJava` instead", "2.13.0")
  def setAsJavaSet[A](s: Set[A]): ju.Set[A] = asJava(s)

  @deprecated("Use `asJava` instead", "2.13.0")
  def mutableMapAsJavaMap[K, V](m: mutable.Map[K, V]): ju.Map[K, V] = asJava(m)

  @deprecated("Use `asJava` instead", "2.13.0")
  def mapAsJavaMap[K, V](m: Map[K, V]): ju.Map[K, V] = asJava(m)

  @deprecated("Use `asJava` instead", "2.13.0")
  def mapAsJavaConcurrentMap[K, V](m: concurrent.Map[K, V]): juc.ConcurrentMap[K, V] = asJava(m)


  @deprecated("Use `asScala` instead", "2.13.0")
  def asScalaIterator[A](i: ju.Iterator[A]): Iterator[A] = asScala(i)

  @deprecated("Use `asScala` instead", "2.13.0")
  def enumerationAsScalaIterator[A](i: ju.Enumeration[A]): Iterator[A] = asScala(i)

  @deprecated("Use `asScala` instead", "2.13.0")
  def iterableAsScalaIterable[A](i: jl.Iterable[A]): Iterable[A] = asScala(i)

  @deprecated("Use `asScala` instead", "2.13.0")
  def collectionAsScalaIterable[A](i: ju.Collection[A]): Iterable[A] = asScala(i)

  @deprecated("Use `asScala` instead", "2.13.0")
  def asScalaBuffer[A](l: ju.List[A]): mutable.Buffer[A] = asScala(l)

  @deprecated("Use `asScala` instead", "2.13.0")
  def asScalaSet[A](s: ju.Set[A]): mutable.Set[A] = asScala(s)

  @deprecated("Use `asScala` instead", "2.13.0")
  def mapAsScalaMap[A, B](m: ju.Map[A, B]): mutable.Map[A, B] = asScala(m)

  @deprecated("Use `asScala` instead", "2.13.0")
  def mapAsScalaConcurrentMap[A, B](m: juc.ConcurrentMap[A, B]): concurrent.Map[A, B] = asScala(m)

  @deprecated("Use `asScala` instead", "2.13.0")
  def dictionaryAsScalaMap[A, B](p: ju.Dictionary[A, B]): mutable.Map[A, B] = asScala(p)

  @deprecated("Use `asScala` instead", "2.13.0")
  def propertiesAsScalaMap(p: ju.Properties): mutable.Map[String, String] = asScala(p)
}
