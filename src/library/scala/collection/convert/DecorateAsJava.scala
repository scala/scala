/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package convert

import java.{ lang => jl, util => ju }, java.util.{ concurrent => juc }
import Decorators._
import WrapAsJava._
import scala.language.implicitConversions


/** A collection of decorators that allow converting between
 *  Scala and Java collections using `asScala` and `asJava` methods.
 *
 *  The following conversions are supported via `asJava`, `asScala`
 *
 *  - `scala.collection.Iterable` <=> `java.lang.Iterable`
 *  - `scala.collection.Iterator` <=> `java.util.Iterator`
 *  - `scala.collection.mutable.Buffer` <=> `java.util.List`
 *  - `scala.collection.mutable.Set` <=> `java.util.Set`
 *  - `scala.collection.mutable.Map` <=> `java.util.Map`
 *  - `scala.collection.mutable.concurrent.Map` <=> `java.util.concurrent.ConcurrentMap`
 *
 *  In all cases, converting from a source type to a target type and back
 *  again will return the original source object, e.g.
 *  {{{
 *    import scala.collection.JavaConverters._
 *
 *    val sl = new scala.collection.mutable.ListBuffer[Int]
 *    val jl : java.util.List[Int] = sl.asJava
 *    val sl2 : scala.collection.mutable.Buffer[Int] = jl.asScala
 *    assert(sl eq sl2)
 *  }}}
 *  The following conversions are also supported, but the
 *  direction from Scala to Java is done by the more specifically named methods:
 *  `asJavaCollection`, `asJavaEnumeration`, `asJavaDictionary`.
 *
 *  - `scala.collection.Iterable` <=> `java.util.Collection`
 *  - `scala.collection.Iterator` <=> `java.util.Enumeration`
 *  - `scala.collection.mutable.Map` <=> `java.util.Dictionary`
 *
 *  In addition, the following one way conversions are provided via `asJava`:
 *
 *  - `scala.collection.Seq` => `java.util.List`
 *  - `scala.collection.mutable.Seq` => `java.util.List`
 *  - `scala.collection.Set` => `java.util.Set`
 *  - `scala.collection.Map` => `java.util.Map`
 *
 *  The following one way conversion is provided via `asScala`:
 *
 *  - `java.util.Properties` => `scala.collection.mutable.Map`
 *
 *  @since  2.8.1
 */
trait DecorateAsJava {
  /**
   * Adds an `asJava` method that implicitly converts a Scala `Iterator` to a
   * Java `Iterator`. The returned Java `Iterator` is backed by the provided Scala
   * `Iterator` and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   *
   * If the Scala `Iterator` was previously obtained from an implicit or explicit
   * call of `asIterator(java.util.Iterator)` then the original Java `Iterator`
   * will be returned by the `asJava` method.
   *
   * @param i The `Iterator` to be converted.
   * @return An object with an `asJava` method that returns a Java `Iterator` view of the argument.
   */
  implicit def asJavaIteratorConverter[A](i : Iterator[A]): AsJava[ju.Iterator[A]] =
    new AsJava(asJavaIterator(i))

  /**
   * Adds an `asJavaEnumeration` method that implicitly converts a Scala
   * `Iterator` to a Java `Enumeration`. The returned Java `Enumeration` is
   * backed by the provided Scala `Iterator` and any side-effects of using
   * it via the Java interface will be visible via the Scala interface and
   * vice versa.
   *
   * If the Scala `Iterator` was previously obtained from an implicit or
   * explicit call of `asIterator(java.util.Enumeration)` then the
   * original Java `Enumeration` will be returned.
   *
   * @param i The `Iterator` to be converted.
   * @return An object with an `asJavaEnumeration` method that returns a Java
   *         `Enumeration` view of the argument.
   */
  implicit def asJavaEnumerationConverter[A](i : Iterator[A]): AsJavaEnumeration[A] =
    new AsJavaEnumeration(i)

  /**
   * Adds an `asJava` method that implicitly converts a Scala `Iterable` to
   * a Java `Iterable`.
   *
   * The returned Java `Iterable` is backed by the provided Scala `Iterable`
   * and any side-effects of using it via the Java interface will be visible
   * via the Scala interface and vice versa.
   *
   * If the Scala `Iterable` was previously obtained from an implicit or
   * explicit call of `asIterable(java.lang.Iterable)` then the original
   * Java `Iterable` will be returned.
   *
   * @param i The `Iterable` to be converted.
   * @return An object with an `asJavaCollection` method that returns a Java
   *         `Iterable` view of the argument.
   */
  implicit def asJavaIterableConverter[A](i : Iterable[A]): AsJava[jl.Iterable[A]] =
    new AsJava(asJavaIterable(i))

  /**
   * Adds an `asJavaCollection` method that implicitly converts a Scala
   * `Iterable` to an immutable Java `Collection`.
   *
   * If the Scala `Iterable` was previously obtained from an implicit or
   * explicit call of `asSizedIterable(java.util.Collection)` then the
   * original Java `Collection` will be returned.
   *
   * @param i The `SizedIterable` to be converted.
   * @return An object with an `asJava` method that returns a Java
   *         `Collection` view of the argument.
   */
  implicit def asJavaCollectionConverter[A](i : Iterable[A]): AsJavaCollection[A] =
    new AsJavaCollection(i)

  /**
   * Adds an `asJava` method that implicitly converts a Scala mutable `Buffer`
   * to a Java `List`.
   *
   * The returned Java `List` is backed by the provided Scala `Buffer` and any
   * side-effects of using it via the Java interface will be visible via the
   * Scala interface and vice versa.
   *
   * If the Scala `Buffer` was previously obtained from an implicit or explicit
   * call of `asBuffer(java.util.List)` then the original Java `List` will be
   * returned.
   *
   * @param b The `Buffer` to be converted.
   * @return An object with an `asJava` method that returns a Java `List` view
   *         of the argument.
   */
  implicit def bufferAsJavaListConverter[A](b : mutable.Buffer[A]): AsJava[ju.List[A]] =
    new AsJava(bufferAsJavaList(b))

  /**
   * Adds an `asJava` method that implicitly converts a Scala mutable `Seq`
   * to a Java `List`.
   *
   * The returned Java `List` is backed by the provided Scala `Seq` and any
   * side-effects of using it via the Java interface will be visible via the
   * Scala interface and vice versa.
   *
   * If the Scala `Seq` was previously obtained from an implicit or explicit
   * call of `asSeq(java.util.List)` then the original Java `List` will be
   * returned.
   *
   * @param b The `Seq` to be converted.
   * @return An object with an `asJava` method that returns a Java `List`
   *         view of the argument.
   */
  implicit def mutableSeqAsJavaListConverter[A](b : mutable.Seq[A]): AsJava[ju.List[A]] =
    new AsJava(mutableSeqAsJavaList(b))

  /**
   * Adds an `asJava` method that implicitly converts a Scala `Seq` to a
   * Java `List`.
   *
   * The returned Java `List` is backed by the provided Scala `Seq` and any
   * side-effects of using it via the Java interface will be visible via the
   * Scala interface and vice versa.
   *
   * If the Scala `Seq` was previously obtained from an implicit or explicit
   * call of `asSeq(java.util.List)` then the original Java `List` will be
   * returned.
   *
   * @param b The `Seq` to be converted.
   * @return An object with an `asJava` method that returns a Java `List`
   *         view of the argument.
   */
  implicit def seqAsJavaListConverter[A](b : Seq[A]): AsJava[ju.List[A]] =
    new AsJava(seqAsJavaList(b))

  /**
   * Adds an `asJava` method that implicitly converts a Scala mutable `Set`>
   * to a Java `Set`.
   *
   * The returned Java `Set` is backed by the provided Scala `Set` and any
   * side-effects of using it via the Java interface will be visible via
   * the Scala interface and vice versa.
   *
   * If the Scala `Set` was previously obtained from an implicit or explicit
   * call of `asSet(java.util.Set)` then the original Java `Set` will be
   * returned.
   *
   * @param s The `Set` to be converted.
   * @return An object with an `asJava` method that returns a Java `Set` view
   *         of the argument.
   */
  implicit def mutableSetAsJavaSetConverter[A](s : mutable.Set[A]): AsJava[ju.Set[A]] =
    new AsJava(mutableSetAsJavaSet(s))

  /**
   * Adds an `asJava` method that implicitly converts a Scala `Set` to a
   * Java `Set`.
   *
   * The returned Java `Set` is backed by the provided Scala `Set` and any
   * side-effects of using it via the Java interface will be visible via
   * the Scala interface and vice versa.
   *
   * If the Scala `Set` was previously obtained from an implicit or explicit
   * call of `asSet(java.util.Set)` then the original Java `Set` will be
   * returned.
   *
   * @param s The `Set` to be converted.
   * @return An object with an `asJava` method that returns a Java `Set` view
   *         of the argument.
   */
  implicit def setAsJavaSetConverter[A](s : Set[A]): AsJava[ju.Set[A]] =
    new AsJava(setAsJavaSet(s))

  /**
   * Adds an `asJava` method that implicitly converts a Scala mutable `Map`
   * to a Java `Map`.
   *
   * The returned Java `Map` is backed by the provided Scala `Map` and any
   * side-effects of using it via the Java interface will be visible via the
   * Scala interface and vice versa.
   *
   * If the Scala `Map` was previously obtained from an implicit or explicit
   * call of `asMap(java.util.Map)` then the original Java `Map` will be
   * returned.
   *
   * @param m The `Map` to be converted.
   * @return An object with an `asJava` method that returns a Java `Map` view
   *         of the argument.
   */
  implicit def mutableMapAsJavaMapConverter[A, B](m : mutable.Map[A, B]): AsJava[ju.Map[A, B]] =
    new AsJava(mutableMapAsJavaMap(m))

  /**
   * Adds an `asJavaDictionary` method that implicitly converts a Scala
   * mutable `Map` to a Java `Dictionary`.
   *
   * The returned Java `Dictionary` is backed by the provided Scala
   * `Dictionary` and any side-effects of using it via the Java interface
   * will be visible via the Scala interface and vice versa.
   *
   * If the Scala `Dictionary` was previously obtained from an implicit or
   * explicit call of `asMap(java.util.Dictionary)` then the original
   * Java `Dictionary` will be returned.
   *
   * @param m The `Map` to be converted.
   * @return An object with an `asJavaDictionary` method that returns a
   *         Java `Dictionary` view of the argument.
   */
  implicit def asJavaDictionaryConverter[A, B](m : mutable.Map[A, B]): AsJavaDictionary[A, B] =
    new AsJavaDictionary(m)

  /**
   * Adds an `asJava` method that implicitly converts a Scala `Map` to
   * a Java `Map`.
   *
   * The returned Java `Map` is backed by the provided Scala `Map` and any
   * side-effects of using it via the Java interface will be visible via
   * the Scala interface and vice versa.
   *
   * If the Scala `Map` was previously obtained from an implicit or explicit
   * call of `asMap(java.util.Map)` then the original Java `Map` will be
   * returned.
   *
   * @param m The `Map` to be converted.
   * @return An object with an `asJava` method that returns a Java `Map` view
   *         of the argument.
   */
  implicit def mapAsJavaMapConverter[A, B](m : Map[A, B]): AsJava[ju.Map[A, B]] =
    new AsJava(mapAsJavaMap(m))

  /**
   * Adds an `asJava` method that implicitly converts a Scala mutable
   * `concurrent.Map` to a Java `ConcurrentMap`.
   *
   * The returned Java `ConcurrentMap` is backed by the provided Scala
   * `concurrent.Map` and any side-effects of using it via the Java interface
   * will be visible via the Scala interface and vice versa.
   *
   * If the Scala `concurrent.Map` was previously obtained from an implicit or
   * explicit call of `asConcurrentMap(java.util.concurrent.ConcurrentMap)`
   * then the original Java `ConcurrentMap` will be returned.
   *
   * @param m The Scala `concurrent.Map` to be converted.
   * @return An object with an `asJava` method that returns a Java
   *         `ConcurrentMap` view of the argument.
   */
  implicit def mapAsJavaConcurrentMapConverter[A, B](m: concurrent.Map[A, B]): AsJava[juc.ConcurrentMap[A, B]] =
    new AsJava(mapAsJavaConcurrentMap(m))
}
