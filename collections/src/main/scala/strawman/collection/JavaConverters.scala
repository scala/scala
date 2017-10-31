/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2016, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package strawman.collection

import convert._

/** A variety of decorators that enable converting between
 *  Scala and Java collections using extension methods, `asScala` and `asJava`.
 *
 *  The extension methods return adapters for the corresponding API.
 *
 *  The following conversions are supported via `asScala` and `asJava`:
 *{{{
 *    strawman.collection.Iterable       <=> java.lang.Iterable
 *    strawman.collection.Iterator       <=> java.util.Iterator
 *    strawman.collection.mutable.Buffer <=> java.util.List
 *    strawman.collection.mutable.Set    <=> java.util.Set
 *    strawman.collection.mutable.Map    <=> java.util.Map
 *    strawman.collection.concurrent.Map <=> java.util.concurrent.ConcurrentMap
 *}}}
 *  The following conversions are supported via `asScala` and through
 *  specially-named extension methods to convert to Java collections, as shown:
 *{{{
 *    strawman.collection.Iterable    <=> java.util.Collection   (via asJavaCollection)
 *    strawman.collection.Iterator    <=> java.util.Enumeration  (via asJavaEnumeration)
 *    strawman.collection.mutable.Map <=> java.util.Dictionary   (via asJavaDictionary)
 *}}}
 *  In addition, the following one-way conversions are provided via `asJava`:
 *{{{
 *    strawman.collection.Seq         => java.util.List
 *    strawman.collection.mutable.Seq => java.util.List
 *    strawman.collection.Set         => java.util.Set
 *    strawman.collection.Map         => java.util.Map
 *}}}
 *  The following one way conversion is provided via `asScala`:
 *{{{
 *    java.util.Properties => strawman.collection.mutable.Map
 *}}}
 *  In all cases, converting from a source type to a target type and back
 *  again will return the original source object. For example:
 *  {{{
 *    import strawman.collection.JavaConverters._
 *
 *    val source = new strawman.collection.mutable.ListBuffer[Int]
 *    val target: java.util.List[Int] = source.asJava
 *    val other: strawman.collection.mutable.Buffer[Int] = target.asScala
 *    assert(source eq other)
 *  }}}
 *  Alternatively, the conversion methods have descriptive names and can be invoked explicitly.
 *  {{{
 *    scala> val vs = java.util.Arrays.asList("hi", "bye")
 *    vs: java.util.List[String] = [hi, bye]
 *
 *    scala> val ss = asScalaIterator(vs.iterator)
 *    ss: Iterator[String] = non-empty iterator
 *
 *    scala> .toList
 *    res0: List[String] = List(hi, bye)
 *
 *    scala> val ss = asScalaBuffer(vs)
 *    ss: strawman.collection.mutable.Buffer[String] = Buffer(hi, bye)
 *  }}}
 *
 *  @since  2.8.1
 */
object JavaConverters extends DecorateAsJava with DecorateAsScala
