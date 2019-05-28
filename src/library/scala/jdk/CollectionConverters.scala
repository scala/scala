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

package scala.jdk

import scala.collection.convert.{AsJavaExtensions, AsScalaExtensions}

/** This object provides extension methods that convert between Scala and Java collections.
  *
  * When writing Java code, use the explicit conversion methods defined in
  * [[javaapi.CollectionConverters]] instead.
  *
  * Note: to create [[java.util.stream.Stream Java Streams]] that operate on Scala collections
  * (sequentially or in parallel), use [[StreamConverters]].
  *
  * {{{
  *   import scala.jdk.CollectionConverters._
  *   val s: java.util.Set[String] = Set("one", "two").asJava
  * }}}
  *
  * The conversions return adapters for the corresponding API, i.e., the collections are wrapped,
  * not converted. Changes to the original collection are reflected in the view, and vice versa:
  *
  * {{{
  *   scala> import scala.jdk.CollectionConverters._
  *
  *   scala> val s = collection.mutable.Set("one")
  *   s: scala.collection.mutable.Set[String] = HashSet(one)
  *
  *   scala> val js = s.asJava
  *   js: java.util.Set[String] = [one]
  *
  *   scala> js.add("two")
  *
  *   scala> s
  *   res2: scala.collection.mutable.Set[String] = HashSet(two, one)
  * }}}
  *
  * The following conversions are supported via `asScala` and `asJava`:
  *
  * {{{
  *   scala.collection.Iterable       <=> java.lang.Iterable
  *   scala.collection.Iterator       <=> java.util.Iterator
  *   scala.collection.mutable.Buffer <=> java.util.List
  *   scala.collection.mutable.Set    <=> java.util.Set
  *   scala.collection.mutable.Map    <=> java.util.Map
  *   scala.collection.concurrent.Map <=> java.util.concurrent.ConcurrentMap
  * }}}
  *
  * The following conversions are supported via `asScala` and through
  * specially-named extension methods to convert to Java collections, as shown:
  *
  * {{{
  *   scala.collection.Iterable    <=> java.util.Collection   (via asJavaCollection)
  *   scala.collection.Iterator    <=> java.util.Enumeration  (via asJavaEnumeration)
  *   scala.collection.mutable.Map <=> java.util.Dictionary   (via asJavaDictionary)
  * }}}
  *
  * In addition, the following one-way conversions are provided via `asJava`:
  *
  * {{{
  *   scala.collection.Seq         => java.util.List
  *   scala.collection.mutable.Seq => java.util.List
  *   scala.collection.Set         => java.util.Set
  *   scala.collection.Map         => java.util.Map
  * }}}
  *
  * The following one way conversion is provided via `asScala`:
  *
  * {{{
  *   java.util.Properties => scala.collection.mutable.Map
  * }}}
  *
  * In all cases, converting from a source type to a target type and back
  * again will return the original source object. For example:
  *
  * {{{
  *   import scala.jdk.CollectionConverters._
  *
  *   val source = new scala.collection.mutable.ListBuffer[Int]
  *   val target: java.util.List[Int] = source.asJava
  *   val other: scala.collection.mutable.Buffer[Int] = target.asScala
  *   assert(source eq other)
  * }}}
  */
object CollectionConverters extends AsJavaExtensions with AsScalaExtensions
