/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection

/** A collection of decorators that allow to convert between
 *  Scala and Java collections using `asScala` and `asJava` methods.
 *
 *  The following conversions are supported via `asJava`, `asScala`
 *
 *  - `scala.collection.Iterable` <=> `java.lang.Iterable`
 *  - `scala.collection.Iterator` <=> `java.util.Iterator`
 *  - `scala.collection.mutable.Buffer` <=> `java.util.List`
 *  - `scala.collection.mutable.Set` <=> `java.util.Set`
 *  - `scala.collection.mutable.Map` <=> `java.util.Map`
 *  - `scala.collection.mutable.ConcurrentMap` <=> `java.util.concurrent.ConcurrentMap`
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
 *  The following conversions also are supported, but the
 *  direction Scala to Java is done my a more specifically named method:
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
 *  @author Martin Odersky
 *  @since  2.8.1
 */

trait JavaConverters {
  import java.{ lang => jl, util => ju }
  import java.util.{ concurrent => juc }
  import JavaConversions._

  // TODO: I cleaned all this documentation up in JavaConversions, but the
  // documentation in here is basically the pre-cleaned-up version with minor
  // additions.  Would be nice to have in one place.

  // Conversion decorator classes

  /** Generic class containing the `asJava` converter method */
  class AsJava[C](op: => C) {
    /** Converts a Scala collection to the corresponding Java collection */
    def asJava: C = op
  }

  /** Generic class containing the `asScala` converter method */
  class AsScala[C](op: => C) {
    /** Converts a Java collection to the corresponding Scala collection */
    def asScala: C = op
  }

  /** Generic class containing the `asJavaCollection` converter method */
  class AsJavaCollection[A](i: Iterable[A]) {
    /** Converts a Scala `Iterable` to a Java `Collection` */
    def asJavaCollection: ju.Collection[A] = JavaConversions.asJavaCollection(i)
  }

  /** Generic class containing the `asJavaEnumeration` converter method */
  class AsJavaEnumeration[A](i: Iterator[A]) {
    /** Converts a Scala `Iterator` to a Java `Enumeration` */
    def asJavaEnumeration: ju.Enumeration[A] = JavaConversions.asJavaEnumeration(i)
  }

  /** Generic class containing the `asJavaDictionary` converter method */
  class AsJavaDictionary[A, B](m : mutable.Map[A, B]) {
    /** Converts a Scala `Map` to a Java `Dictionary` */
    def asJavaDictionary: ju.Dictionary[A, B] = JavaConversions.asJavaDictionary(m)
  }

  // Scala => Java

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

  @deprecated("Use bufferAsJavaListConverter instead", "2.9.0")
  def asJavaListConverter[A](b : mutable.Buffer[A]): AsJava[ju.List[A]] = bufferAsJavaListConverter(b)
  @deprecated("Use mutableSeqAsJavaListConverter instead", "2.9.0")
  def asJavaListConverter[A](b : mutable.Seq[A]): AsJava[ju.List[A]] = mutableSeqAsJavaListConverter(b)
  @deprecated("Use seqAsJavaListConverter instead", "2.9.0")
  def asJavaListConverter[A](b : Seq[A]): AsJava[ju.List[A]] = seqAsJavaListConverter(b)

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

  @deprecated("Use mutableSetAsJavaSetConverter instead", "2.9.0")
  def asJavaSetConverter[A](s : mutable.Set[A]): AsJava[ju.Set[A]] = mutableSetAsJavaSetConverter(s)

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

  @deprecated("Use setAsJavaSetConverter instead", "2.9.0")
  def asJavaSetConverter[A](s : Set[A]): AsJava[ju.Set[A]] = setAsJavaSetConverter(s)

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

  @deprecated("use mutableMapAsJavaMapConverter instead", "2.9.0")
  def asJavaMapConverter[A, B](m : mutable.Map[A, B]): AsJava[ju.Map[A, B]] = mutableMapAsJavaMapConverter(m)

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

  @deprecated("Use mapAsJavaMapConverter instead", "2.9.0")
  def asJavaMapConverter[A, B](m : Map[A, B]): AsJava[ju.Map[A, B]] = mapAsJavaMapConverter(m)

  /**
   * Adds an `asJava` method that implicitly converts a Scala mutable
   * `ConcurrentMap` to a Java `ConcurrentMap`.
   *
   * The returned Java `ConcurrentMap` is backed by the provided Scala
   * `ConcurrentMap` and any side-effects of using it via the Java interface
   * will be visible via the Scala interface and vice versa.
   *
   * If the Scala `ConcurrentMap` was previously obtained from an implicit or
   * explicit call of `asConcurrentMap(java.util.concurrect.ConcurrentMap)`
   * then the original Java `ConcurrentMap` will be returned.
   *
   * @param m The `ConcurrentMap` to be converted.
   * @return An object with an `asJava` method that returns a Java
   *         `ConcurrentMap` view of the argument.
   */
  implicit def asJavaConcurrentMapConverter[A, B](m: mutable.ConcurrentMap[A, B]): AsJava[juc.ConcurrentMap[A, B]] =
    new AsJava(asJavaConcurrentMap(m))

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

  @deprecated("Use iterableAsScalaIterableConverter instead", "2.9.0")
  def asScalaIterableConverter[A](i : jl.Iterable[A]): AsScala[Iterable[A]] = iterableAsScalaIterableConverter(i)

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

  @deprecated("Use collectionAsScalaIterableConverter instead", "2.9.0")
  def asScalaIterableConverter[A](i : ju.Collection[A]): AsScala[Iterable[A]] = collectionAsScalaIterableConverter(i)

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

  @deprecated("Use mapAsScalaMapConverter instead", "2.9.0")
  def asScalaMapConverter[A, B](m : ju.Map[A, B]): AsScala[mutable.Map[A, B]] = mapAsScalaMapConverter(m)

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
  implicit def asScalaConcurrentMapConverter[A, B](m: juc.ConcurrentMap[A, B]): AsScala[mutable.ConcurrentMap[A, B]] =
    new AsScala(asScalaConcurrentMap(m))

  /**
   * Adds an `asScala` method that implicitly converts a Java `Dictionary`
   * to a Scala mutable `Map[String, String]`. The returned Scala
   * `Map[String, String]` is backed by the provided Java `Dictionary` and
   * any side-effects of using it via the Scala interface will be visible via
   * the Java interface and vice versa.
   *
   * @param m The `Dictionary` to be converted.
   * @return An object with an `asScala` method that returns a Scala mutable
   *         `Map[String, String]` view of the argument.
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
   * @param m The `Properties` to be converted.
   * @return An object with an `asScala` method that returns a Scala mutable
   *         `Map[String, String]` view of the argument.
   */
  implicit def propertiesAsScalaMapConverter(p: ju.Properties): AsScala[mutable.Map[String, String]] =
    new AsScala(propertiesAsScalaMap(p))

  @deprecated("Use propertiesAsScalaMapConverter instead", "2.9.0")
  def asScalaMapConverter(p: ju.Properties): AsScala[mutable.Map[String, String]] =
    propertiesAsScalaMapConverter(p)

}

object JavaConverters extends JavaConverters