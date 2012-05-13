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
import Decorators._
import WrapAsScala._
import language.implicitConversions

trait DecorateAsScala {
  /**
   * Adds an `asScala` method that implicitly converts a Java `Iterator` to
   * a Scala `Iterator`.
   *
   * The returned Scala `Iterator` is backed by the provided Java `Iterator`
   * and any side-effects of using it via the Scala interface will be visible
   * via the Java interface and vice versa.
   *
   * If the Java `Iterator` was previously obtained from an implicit or
   * explicit call of `asIterator(scala.collection.Iterator)` then the
   * original Scala `Iterator` will be returned.
   *
   * @param i The `Iterator` to be converted.
   * @return An object with an `asScala` method that returns a Scala
   *         `Iterator` view of the argument.
   */
  implicit def asScalaIteratorConverter[A](i : ju.Iterator[A]): AsScala[Iterator[A]] =
    new AsScala(asScalaIterator(i))

  /**
   * Adds an `asScala` method that implicitly converts a Java `Enumeration`
   * to a Scala `Iterator`.
   *
   * The returned Scala `Iterator` is backed by the provided Java
   * `Enumeration` and any side-effects of using it via the Scala interface
   * will be visible via the Java interface and vice versa.
   *
   * If the Java `Enumeration` was previously obtained from an implicit or
   * explicit call of `asEnumeration(scala.collection.Iterator)` then the
   * original Scala `Iterator` will be returned.
   *
   * @param i The `Enumeration` to be converted.
   * @return An object with an `asScala` method that returns a Scala
   *         `Iterator` view of the argument.
   */
  implicit def enumerationAsScalaIteratorConverter[A](i : ju.Enumeration[A]): AsScala[Iterator[A]] =
    new AsScala(enumerationAsScalaIterator(i))

  /**
   * Adds an `asScala` method that implicitly converts a Java `Iterable` to
   * a Scala `Iterable`.
   *
   * The returned Scala `Iterable` is backed by the provided Java `Iterable`
   * and any side-effects of using it via the Scala interface will be visible
   * via the Java interface and vice versa.
   *
   * If the Java `Iterable` was previously obtained from an implicit or
   * explicit call of `asIterable(scala.collection.Iterable)` then the original
   * Scala `Iterable` will be returned.
   *
   * @param i The `Iterable` to be converted.
   * @return An object with an `asScala` method that returns a Scala `Iterable`
   *         view of the argument.
   */
  implicit def iterableAsScalaIterableConverter[A](i : jl.Iterable[A]): AsScala[Iterable[A]] =
    new AsScala(iterableAsScalaIterable(i))

  /**
   * Adds an `asScala` method that implicitly converts a Java `Collection` to
   * an Scala `Iterable`.
   *
   * If the Java `Collection` was previously obtained from an implicit or
   * explicit call of `asCollection(scala.collection.SizedIterable)` then
   * the original Scala `SizedIterable` will be returned.
   *
   * @param i The `Collection` to be converted.
   * @return An object with an `asScala` method that returns a Scala
   *        `SizedIterable` view of the argument.
   */
  implicit def collectionAsScalaIterableConverter[A](i : ju.Collection[A]): AsScala[Iterable[A]] =
    new AsScala(collectionAsScalaIterable(i))

  /**
   * Adds an `asScala` method that implicitly converts a Java `List` to a
   * Scala mutable `Buffer`.
   *
   * The returned Scala `Buffer` is backed by the provided Java `List` and
   * any side-effects of using it via the Scala interface will be visible via
   * the Java interface and vice versa.
   *
   * If the Java `List` was previously obtained from an implicit or explicit
   * call of `asList(scala.collection.mutable.Buffer)` then the original
   * Scala `Buffer` will be returned.
   *
   * @param l The `List` to be converted.
   * @return An object with an `asScala` method that returns a Scala mutable
   *        `Buffer` view of the argument.
   */
  implicit def asScalaBufferConverter[A](l : ju.List[A]): AsScala[mutable.Buffer[A]] =
    new AsScala(asScalaBuffer(l))

  /**
   * Adds an `asScala` method that implicitly converts a Java `Set` to a
   * Scala mutable `Set`.
   *
   * The returned Scala `Set` is backed by the provided Java `Set` and any
   * side-effects of using it via the Scala interface will be visible via
   * the Java interface and vice versa.
   *
   * If the Java `Set` was previously obtained from an implicit or explicit
   * call of `asSet(scala.collection.mutable.Set)` then the original
   * Scala `Set` will be returned.
   *
   * @param s The `Set` to be converted.
   * @return An object with an `asScala` method that returns a Scala mutable
   *         `Set` view of the argument.
   */
  implicit def asScalaSetConverter[A](s : ju.Set[A]): AsScala[mutable.Set[A]] =
    new AsScala(asScalaSet(s))

  /**
   * Adds an `asScala` method that implicitly converts a Java `Map` to a Scala
   * mutable `Map`. The returned Scala `Map` is backed by the provided Java
   * `Map` and any side-effects of using it via the Scala interface will
   * be visible via the Java interface and vice versa.
   *
   * If the Java `Map` was previously obtained from an implicit or explicit
   * call of `asMap(scala.collection.mutable.Map)` then the original
   * Scala `Map` will be returned.
   *
   * @param m The `Map` to be converted.
   * @return An object with an `asScala` method that returns a Scala mutable
   *         `Map` view of the argument.
   */
  implicit def mapAsScalaMapConverter[A, B](m : ju.Map[A, B]): AsScala[mutable.Map[A, B]] =
    new AsScala(mapAsScalaMap(m))

  /**
   * Adds an `asScala` method that implicitly converts a Java `ConcurrentMap`
   * to a Scala mutable `ConcurrentMap`. The returned Scala `ConcurrentMap` is
   * backed by the provided Java `ConcurrentMap` and any side-effects of using
   * it via the Scala interface will be visible via the Java interface and
   * vice versa.
   *
   * If the Java `ConcurrentMap` was previously obtained from an implicit or
   * explicit call of `asConcurrentMap(scala.collection.mutable.ConcurrentMap)`
   * then the original Scala `ConcurrentMap` will be returned.
   *
   * @param m The `ConcurrentMap` to be converted.
   * @return An object with an `asScala` method that returns a Scala mutable
   *         `ConcurrentMap` view of the argument.
   */
  @deprecated("Use `mapAsScalaConcurrentMapConverter` instead, and use `concurrent.Map` instead of `ConcurrentMap`.", "2.10.0")
  def asScalaConcurrentMapConverter[A, B](m: juc.ConcurrentMap[A, B]): AsScala[mutable.ConcurrentMap[A, B]] =
    new AsScala(asScalaConcurrentMap(m))

  /**
   * Adds an `asScala` method that implicitly converts a Java `ConcurrentMap`
   * to a Scala mutable `concurrent.Map`. The returned Scala `concurrent.Map` is
   * backed by the provided Java `ConcurrentMap` and any side-effects of using
   * it via the Scala interface will be visible via the Java interface and
   * vice versa.
   *
   * If the Java `ConcurrentMap` was previously obtained from an implicit or
   * explicit call of `mapAsScalaConcurrentMap(scala.collection.mutable.ConcurrentMap)`
   * then the original Scala `concurrent.Map` will be returned.
   *
   * @param m The `ConcurrentMap` to be converted.
   * @return An object with an `asScala` method that returns a Scala mutable
   *         `concurrent.Map` view of the argument.
   */
  implicit def mapAsScalaConcurrentMapConverter[A, B](m: juc.ConcurrentMap[A, B]): AsScala[concurrent.Map[A, B]] =
    new AsScala(mapAsScalaConcurrentMap(m))

  /**
   * Adds an `asScala` method that implicitly converts a Java `Dictionary`
   * to a Scala mutable `Map[String, String]`. The returned Scala
   * `Map[String, String]` is backed by the provided Java `Dictionary` and
   * any side-effects of using it via the Scala interface will be visible via
   * the Java interface and vice versa.
   *
   * @param p The `Dictionary` to be converted.
   * @return  An object with an `asScala` method that returns a Scala mutable
   *          `Map[String, String]` view of the argument.
   */
  implicit def dictionaryAsScalaMapConverter[A, B](p: ju.Dictionary[A, B]): AsScala[mutable.Map[A, B]] =
    new AsScala(dictionaryAsScalaMap(p))

  /**
   * Adds an `asScala` method that implicitly converts a Java `Properties`
   * to a Scala mutable `Map[String, String]`. The returned Scala
   * `Map[String, String]` is backed by the provided Java `Properties` and
   * any side-effects of using it via the Scala interface will be visible via
   * the Java interface and vice versa.
   *
   * @param p The `Properties` to be converted.
   * @return  An object with an `asScala` method that returns a Scala mutable
   *          `Map[String, String]` view of the argument.
   */
  implicit def propertiesAsScalaMapConverter(p: ju.Properties): AsScala[mutable.Map[String, String]] =
    new AsScala(propertiesAsScalaMap(p))
}
