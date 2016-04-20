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
import Decorators._
import scala.language.implicitConversions

/** Defines `asScala` extension methods for [[JavaConverters]]. */
trait DecorateAsScala extends AsScalaConverters {
  /**
   * Adds an `asScala` method that implicitly converts a Java `Iterator` to a Scala `Iterator`.
   * See [[asScalaIterator]].
   */
  implicit def asScalaIteratorConverter[A](i : ju.Iterator[A]): AsScala[Iterator[A]] =
    new AsScala(asScalaIterator(i))

  /**
   * Adds an `asScala` method that implicitly converts a Java `Enumeration` to a Scala `Iterator`.
   * See [[enumerationAsScalaIterator]].
   */
  implicit def enumerationAsScalaIteratorConverter[A](i : ju.Enumeration[A]): AsScala[Iterator[A]] =
    new AsScala(enumerationAsScalaIterator(i))

  /**
   * Adds an `asScala` method that implicitly converts a Java `Iterable` to a Scala `Iterable`.
   * See [[iterableAsScalaIterable]].
   */
  implicit def iterableAsScalaIterableConverter[A](i : jl.Iterable[A]): AsScala[Iterable[A]] =
    new AsScala(iterableAsScalaIterable(i))

  /**
   * Adds an `asScala` method that implicitly converts a Java `Collection` to an Scala `Iterable`.
   * See [[collectionAsScalaIterable]].
   */
  implicit def collectionAsScalaIterableConverter[A](i : ju.Collection[A]): AsScala[Iterable[A]] =
    new AsScala(collectionAsScalaIterable(i))

  /**
   * Adds an `asScala` method that implicitly converts a Java `List` to a Scala mutable `Buffer`.
   * See [[asScalaBuffer]].
   */
  implicit def asScalaBufferConverter[A](l : ju.List[A]): AsScala[mutable.Buffer[A]] =
    new AsScala(asScalaBuffer(l))

  /**
   * Adds an `asScala` method that implicitly converts a Java `Set` to a Scala mutable `Set`.
   * See [[asScalaSet]].
   */
  implicit def asScalaSetConverter[A](s : ju.Set[A]): AsScala[mutable.Set[A]] =
    new AsScala(asScalaSet(s))

  /**
   * Adds an `asScala` method that implicitly converts a Java `Map` to a Scala mutable `Map`.
   * See [[mapAsScalaMap]].
   */
  implicit def mapAsScalaMapConverter[A, B](m : ju.Map[A, B]): AsScala[mutable.Map[A, B]] =
    new AsScala(mapAsScalaMap(m))

  /**
   * Adds an `asScala` method that implicitly converts a Java `ConcurrentMap` to a Scala mutable
   * `concurrent.Map`. See [[mapAsScalaConcurrentMap]].
   */
  implicit def mapAsScalaConcurrentMapConverter[A, B](m: juc.ConcurrentMap[A, B]): AsScala[concurrent.Map[A, B]] =
    new AsScala(mapAsScalaConcurrentMap(m))

  /**
   * Adds an `asScala` method that implicitly converts a Java `Dictionary` to a Scala mutable `Map`.
   * See [[dictionaryAsScalaMap]].
   */
  implicit def dictionaryAsScalaMapConverter[A, B](p: ju.Dictionary[A, B]): AsScala[mutable.Map[A, B]] =
    new AsScala(dictionaryAsScalaMap(p))

  /**
   * Adds an `asScala` method that implicitly converts a Java `Properties` to a Scala mutable
   * `Map[String, String]`. See [[propertiesAsScalaMap]].
   */
  implicit def propertiesAsScalaMapConverter(p: ju.Properties): AsScala[mutable.Map[String, String]] =
    new AsScala(propertiesAsScalaMap(p))
}
