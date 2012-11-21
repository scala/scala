/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection

import convert._

/**   A collection of implicit conversions supporting interoperability between
 *    Scala and Java collections.
 *
 *    The following conversions are supported:
 *{{{
 *    scala.collection.Iterable <=> java.lang.Iterable
 *    scala.collection.Iterable <=> java.util.Collection
 *    scala.collection.Iterator <=> java.util.{ Iterator, Enumeration }
 *    scala.collection.mutable.Buffer <=> java.util.List
 *    scala.collection.mutable.Set <=> java.util.Set
 *    scala.collection.mutable.Map <=> java.util.{ Map, Dictionary }
 *    scala.collection.mutable.ConcurrentMap (deprecated since 2.10) <=> java.util.concurrent.ConcurrentMap
 *    scala.collection.concurrent.Map <=> java.util.concurrent.ConcurrentMap
 *}}}
 *    In all cases, converting from a source type to a target type and back
 *    again will return the original source object, eg.
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
 *  @author Miles Sabin
 *  @author Martin Odersky
 *  @since  2.8
 */
object JavaConversions extends WrapAsScala with WrapAsJava {
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") type ConcurrentMapWrapper[A, B]  = Wrappers.ConcurrentMapWrapper[A, B]
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") type DictionaryWrapper[A, B]     = Wrappers.DictionaryWrapper[A, B]
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") type IterableWrapper[A]          = Wrappers.IterableWrapper[A]
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") type IteratorWrapper[A]          = Wrappers.IteratorWrapper[A]
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") type JCollectionWrapper[A]       = Wrappers.JCollectionWrapper[A]
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") type JConcurrentMapWrapper[A, B] = Wrappers.JConcurrentMapWrapper[A, B]
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") type JDictionaryWrapper[A, B]    = Wrappers.JDictionaryWrapper[A, B]
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") type JEnumerationWrapper[A]      = Wrappers.JEnumerationWrapper[A]
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") type JIterableWrapper[A]         = Wrappers.JIterableWrapper[A]
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") type JIteratorWrapper[A]         = Wrappers.JIteratorWrapper[A]
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") type JListWrapper[A]             = Wrappers.JListWrapper[A]
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") type JMapWrapper[A, B]           = Wrappers.JMapWrapper[A, B]
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") type JPropertiesWrapper          = Wrappers.JPropertiesWrapper
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") type JSetWrapper[A]              = Wrappers.JSetWrapper[A]
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") type MapWrapper[A, B]            = Wrappers.MapWrapper[A, B]
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") type MutableBufferWrapper[A]     = Wrappers.MutableBufferWrapper[A]
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") type MutableMapWrapper[A, B]     = Wrappers.MutableMapWrapper[A, B]
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") type MutableSeqWrapper[A]        = Wrappers.MutableSeqWrapper[A]
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") type MutableSetWrapper[A]        = Wrappers.MutableSetWrapper[A]
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") type SeqWrapper[A]               = Wrappers.SeqWrapper[A]
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") type SetWrapper[A]               = Wrappers.SetWrapper[A]
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") type ToIteratorWrapper[A]        = Wrappers.ToIteratorWrapper[A]
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") val DictionaryWrapper            = Wrappers.DictionaryWrapper
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") val IterableWrapper              = Wrappers.IterableWrapper
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") val IteratorWrapper              = Wrappers.IteratorWrapper
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") val JCollectionWrapper           = Wrappers.JCollectionWrapper
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") val JConcurrentMapWrapper        = Wrappers.JConcurrentMapWrapper
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") val JDictionaryWrapper           = Wrappers.JDictionaryWrapper
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") val JEnumerationWrapper          = Wrappers.JEnumerationWrapper
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") val JIterableWrapper             = Wrappers.JIterableWrapper
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") val JIteratorWrapper             = Wrappers.JIteratorWrapper
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") val JListWrapper                 = Wrappers.JListWrapper
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") val JMapWrapper                  = Wrappers.JMapWrapper
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") val JPropertiesWrapper           = Wrappers.JPropertiesWrapper
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") val JSetWrapper                  = Wrappers.JSetWrapper
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") val MutableBufferWrapper         = Wrappers.MutableBufferWrapper
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") val MutableMapWrapper            = Wrappers.MutableMapWrapper
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") val MutableSeqWrapper            = Wrappers.MutableSeqWrapper
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") val MutableSetWrapper            = Wrappers.MutableSetWrapper
  @deprecated("Use a member of scala.collection.convert.Wrappers", "2.10.0") val SeqWrapper                   = Wrappers.SeqWrapper
}


