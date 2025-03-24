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

package scala.collection

import java.util.{concurrent => juc}
import java.{lang => jl, util => ju}

import scala.collection.convert._
import scala.language.implicitConversions

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
 */
@deprecated("Use `scala.jdk.CollectionConverters` instead", "2.13.0")
object JavaConverters extends AsJavaConverters with AsScalaConverters {
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

  // Deprecated implicit conversions for code that directly imports them

  /**
    * Adds an `asJava` method that implicitly converts a Scala `Iterator` to a Java `Iterator`.
    * @see [[asJavaIterator]]
    */
  implicit def asJavaIteratorConverter[A](i : Iterator[A]): AsJava[ju.Iterator[A]] =
    new AsJava(asJavaIterator(i))

  /**
    * Adds an `asJavaEnumeration` method that implicitly converts a Scala `Iterator` to a Java `Enumeration`.
    * @see [[asJavaEnumeration]]
    */
  implicit def asJavaEnumerationConverter[A](i : Iterator[A]): AsJavaEnumeration[A] =
    new AsJavaEnumeration(i)

  /**
    * Adds an `asJava` method that implicitly converts a Scala `Iterable` to a Java `Iterable`.
    * @see [[asJavaIterable]]
    */
  implicit def asJavaIterableConverter[A](i : Iterable[A]): AsJava[jl.Iterable[A]] =
    new AsJava(asJavaIterable(i))

  /**
    * Adds an `asJavaCollection` method that implicitly converts a Scala `Iterable` to an immutable Java `Collection`.
    * @see [[asJavaCollection]]
    */
  implicit def asJavaCollectionConverter[A](i : Iterable[A]): AsJavaCollection[A] =
    new AsJavaCollection(i)

  /**
    * Adds an `asJava` method that implicitly converts a Scala mutable `Buffer` to a Java `List`.
    * @see [[bufferAsJavaList]]
    */
  implicit def bufferAsJavaListConverter[A](b : mutable.Buffer[A]): AsJava[ju.List[A]] =
    new AsJava(bufferAsJavaList(b))

  /**
    * Adds an `asJava` method that implicitly converts a Scala mutable `Seq` to a Java `List`.
    * @see [[mutableSeqAsJavaList]]
    */
  implicit def mutableSeqAsJavaListConverter[A](b : mutable.Seq[A]): AsJava[ju.List[A]] =
    new AsJava(mutableSeqAsJavaList(b))

  /**
    * Adds an `asJava` method that implicitly converts a Scala `Seq` to a Java `List`.
    * @see [[seqAsJavaList]]
    */
  implicit def seqAsJavaListConverter[A](b : Seq[A]): AsJava[ju.List[A]] =
    new AsJava(seqAsJavaList(b))

  /**
    * Adds an `asJava` method that implicitly converts a Scala mutable `Set` to a Java `Set`.
    * @see [[mutableSetAsJavaSet]]
    */
  implicit def mutableSetAsJavaSetConverter[A](s : mutable.Set[A]): AsJava[ju.Set[A]] =
    new AsJava(mutableSetAsJavaSet(s))

  /**
    * Adds an `asJava` method that implicitly converts a Scala `Set` to a Java `Set`.
    * @see [[setAsJavaSet]]
    */
  implicit def setAsJavaSetConverter[A](s : Set[A]): AsJava[ju.Set[A]] =
    new AsJava(setAsJavaSet(s))

  /**
    * Adds an `asJava` method that implicitly converts a Scala mutable `Map` to a Java `Map`.
    * @see [[mutableMapAsJavaMap]]
    */
  implicit def mutableMapAsJavaMapConverter[K, V](m : mutable.Map[K, V]): AsJava[ju.Map[K, V]] =
    new AsJava(mutableMapAsJavaMap(m))

  /**
    * Adds an `asJavaDictionary` method that implicitly converts a Scala mutable `Map` to a Java `Dictionary`.
    * @see [[asJavaDictionary]]
    */
  implicit def asJavaDictionaryConverter[K, V](m : mutable.Map[K, V]): AsJavaDictionary[K, V] =
    new AsJavaDictionary(m)

  /**
    * Adds an `asJava` method that implicitly converts a Scala `Map` to a Java `Map`.
    * @see [[mapAsJavaMap]]
    */
  implicit def mapAsJavaMapConverter[K, V](m : Map[K, V]): AsJava[ju.Map[K, V]] =
    new AsJava(mapAsJavaMap(m))

  /**
    * Adds an `asJava` method that implicitly converts a Scala mutable `concurrent.Map` to a Java `ConcurrentMap`.
    * @see [[mapAsJavaConcurrentMap]].
    */
  implicit def mapAsJavaConcurrentMapConverter[K, V](m: concurrent.Map[K, V]): AsJava[juc.ConcurrentMap[K, V]] =
    new AsJava(mapAsJavaConcurrentMap(m))


  /**
    * Adds an `asScala` method that implicitly converts a Java `Iterator` to a Scala `Iterator`.
    * @see [[asScalaIterator]]
    */
  implicit def asScalaIteratorConverter[A](i : ju.Iterator[A]): AsScala[Iterator[A]] =
    new AsScala(asScalaIterator(i))

  /**
    * Adds an `asScala` method that implicitly converts a Java `Enumeration` to a Scala `Iterator`.
    * @see [[enumerationAsScalaIterator]]
    */
  implicit def enumerationAsScalaIteratorConverter[A](i : ju.Enumeration[A]): AsScala[Iterator[A]] =
    new AsScala(enumerationAsScalaIterator(i))

  /**
    * Adds an `asScala` method that implicitly converts a Java `Iterable` to a Scala `Iterable`.
    * @see [[iterableAsScalaIterable]]
    */
  implicit def iterableAsScalaIterableConverter[A](i : jl.Iterable[A]): AsScala[Iterable[A]] =
    new AsScala(iterableAsScalaIterable(i))

  /**
    * Adds an `asScala` method that implicitly converts a Java `Collection` to an Scala `Iterable`.
    * @see [[collectionAsScalaIterable]]
    */
  implicit def collectionAsScalaIterableConverter[A](i : ju.Collection[A]): AsScala[Iterable[A]] =
    new AsScala(collectionAsScalaIterable(i))

  /**
    * Adds an `asScala` method that implicitly converts a Java `List` to a Scala mutable `Buffer`.
    * @see [[asScalaBuffer]]
    */
  implicit def asScalaBufferConverter[A](l : ju.List[A]): AsScala[mutable.Buffer[A]] =
    new AsScala(asScalaBuffer(l))

  /**
    * Adds an `asScala` method that implicitly converts a Java `Set` to a Scala mutable `Set`.
    * @see [[asScalaSet]]
    */
  implicit def asScalaSetConverter[A](s : ju.Set[A]): AsScala[mutable.Set[A]] =
    new AsScala(asScalaSet(s))

  /**
    * Adds an `asScala` method that implicitly converts a Java `Map` to a Scala mutable `Map`.
    * @see [[mapAsScalaMap]]
    */
  implicit def mapAsScalaMapConverter[K, V](m : ju.Map[K, V]): AsScala[mutable.Map[K, V]] =
    new AsScala(mapAsScalaMap(m))

  /**
    * Adds an `asScala` method that implicitly converts a Java `ConcurrentMap` to a Scala mutable `concurrent.Map`.
    * @see [[mapAsScalaConcurrentMap]]
    */
  implicit def mapAsScalaConcurrentMapConverter[K, V](m: juc.ConcurrentMap[K, V]): AsScala[concurrent.Map[K, V]] =
    new AsScala(mapAsScalaConcurrentMap(m))

  /**
    * Adds an `asScala` method that implicitly converts a Java `Dictionary` to a Scala mutable `Map`.
    * @see [[dictionaryAsScalaMap]]
    */
  implicit def dictionaryAsScalaMapConverter[K, V](p: ju.Dictionary[K, V]): AsScala[mutable.Map[K, V]] =
    new AsScala(dictionaryAsScalaMap(p))

  /**
    * Adds an `asScala` method that implicitly converts a Java `Properties` to a Scala mutable `Map[String, String]`.
    * @see [[propertiesAsScalaMap]]
    */
  implicit def propertiesAsScalaMapConverter(p: ju.Properties): AsScala[mutable.Map[String, String]] =
    new AsScala(propertiesAsScalaMap(p))


  /** Generic class containing the `asJava` converter method */
  class AsJava[A](op: => A) {
    /** Converts a Scala collection to the corresponding Java collection */
    def asJava: A = op
  }

  /** Generic class containing the `asScala` converter method */
  class AsScala[A](op: => A) {
    /** Converts a Java collection to the corresponding Scala collection */
    def asScala: A = op
  }

  /** Generic class containing the `asJavaCollection` converter method */
  class AsJavaCollection[A](i: Iterable[A]) {
    /** Converts a Scala `Iterable` to a Java `Collection` */
    def asJavaCollection: ju.Collection[A] = JavaConverters.asJavaCollection(i)
  }

  /** Generic class containing the `asJavaEnumeration` converter method */
  class AsJavaEnumeration[A](i: Iterator[A]) {
    /** Converts a Scala `Iterator` to a Java `Enumeration` */
    def asJavaEnumeration: ju.Enumeration[A] = JavaConverters.asJavaEnumeration(i)
  }

  /** Generic class containing the `asJavaDictionary` converter method */
  class AsJavaDictionary[K, V](m : mutable.Map[K, V]) {
    /** Converts a Scala `Map` to a Java `Dictionary` */
    def asJavaDictionary: ju.Dictionary[K, V] = JavaConverters.asJavaDictionary(m)
  }
}
