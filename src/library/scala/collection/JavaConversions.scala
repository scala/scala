/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2016, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection

import convert._

/** A variety of implicit conversions supporting interoperability between
 *  Scala and Java collections.
 *
 *  The following conversions are supported:
 *{{{
 *    scala.collection.Iterable       <=> java.lang.Iterable
 *    scala.collection.Iterable       <=> java.util.Collection
 *    scala.collection.Iterator       <=> java.util.{ Iterator, Enumeration }
 *    scala.collection.mutable.Buffer <=> java.util.List
 *    scala.collection.mutable.Set    <=> java.util.Set
 *    scala.collection.mutable.Map    <=> java.util.{ Map, Dictionary }
 *    scala.collection.concurrent.Map <=> java.util.concurrent.ConcurrentMap
 *}}}
 *  In all cases, converting from a source type to a target type and back
 *  again will return the original source object:
 *
 *{{{
 *    import scala.collection.JavaConversions._
 *
 *    val sl = new scala.collection.mutable.ListBuffer[Int]
 *    val jl : java.util.List[Int] = sl
 *    val sl2 : scala.collection.mutable.Buffer[Int] = jl
 *    assert(sl eq sl2)
 *}}}
 *  In addition, the following one way conversions are provided:
 *
 *{{{
 *    scala.collection.Seq         => java.util.List
 *    scala.collection.mutable.Seq => java.util.List
 *    scala.collection.Set         => java.util.Set
 *    scala.collection.Map         => java.util.Map
 *    java.util.Properties         => scala.collection.mutable.Map[String, String]
 *}}}
 *
 *  The transparent conversions provided here are considered
 *  fragile because they can result in unexpected behavior and performance.
 *
 *  Therefore, this API has been deprecated and `JavaConverters` should be
 *  used instead. `JavaConverters` provides the same conversions, but through
 *  extension methods.
 *
 *  @author Miles Sabin
 *  @author Martin Odersky
 *  @since  2.8
 */
@deprecated("use JavaConverters", since="2.12.0")
object JavaConversions extends WrapAsScala with WrapAsJava
