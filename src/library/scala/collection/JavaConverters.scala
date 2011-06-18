/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection

/** <p>
 *    A collection of decorators that allow to convert between
 *    Scala and Java collections using `asScala` and `asJava` methods.
 *  </p>
 *  <p>
 *    The following conversions are supported via `asJava`, `asScala`
 *  </p>
 *  <ul>
 *    <li><code>scala.collection.Iterable</code> <=> <code>java.lang.Iterable</code></li>
 *    <li><code>scala.collection.Iterator</code> <=> <code>java.util.Iterator</code></li>
 *    <li><code>scala.collection.mutable.Buffer</code> <=> <code>java.util.List</code></li>
 *    <li><code>scala.collection.mutable.Set</code> <=> <code>java.util.Set</code></li>
 *    <li><code>scala.collection.mutable.Map</code> <=> <code>java.util.Map</code></li>
 *    <li><code>scala.collection.mutable.ConcurrentMap</code> <=> <code>java.util.concurrent.ConcurrentMap</code></li>
 *  </ul>
 *  <p>
 *    In all cases, converting from a source type to a target type and back
 *    again will return the original source object, e.g.
 *  </p>
 *  <pre>
 *    <b>import</b> scala.collection.JavaConverters._
 *
 *    <b>val</b> sl = <b>new</b> scala.collection.mutable.ListBuffer[Int]
 *    <b>val</b> jl : java.util.List[Int] = sl.asJava
 *    <b>val</b> sl2 : scala.collection.mutable.Buffer[Int] = jl.asScala
 *    assert(sl eq sl2)g</pre>
 *  <p>
 *  <p>
 *    The following conversions also are supported, but the
 *    direction Scala to Java is done my a more specifically named method:
 *    `asJavaCollection`, `asJavaEnumeration`, `asJavaDictionary`.
 *  </p>
 *  <ul>
 *    <li><code>scala.collection.Iterable</code> <=> <code>java.util.Collection</code></li>
 *    <li><code>scala.collection.Iterator</code> <=> <code>java.util.Enumeration</code></li>
 *    <li><code>scala.collection.mutable.Map</code> <=> <code>java.util.Dictionary</code></li>
 *  </ul>
 *  In addition, the following one way conversions are provided via `asJava`:
 *  </p>
 *  <ul>
 *    <li><code>scala.collection.Seq</code> => <code>java.util.List</code></li>
 *    <li><code>scala.collection.mutable.Seq</code> => <code>java.util.List</code></li>
 *    <li><code>scala.collection.Set</code> => <code>java.util.Set</code></li>
 *    <li><code>scala.collection.Map</code> => <code>java.util.Map</code></li>
 *  </ul>
 *
 *  @author Martin Odersky
 *  @since  2.8.1
 */
object JavaConverters {
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
   * Adds an `asJava` method that implicitly converts a Scala <code>Iterator</code> to a Java <code>Iterator</code>.
   * The returned Java <code>Iterator</code> is backed by the provided Scala
   * <code>Iterator</code> and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   * <p>
   * If the Scala <code>Iterator</code> was previously obtained from an implicit or
   * explicit call of <code>asIterator(java.util.Iterator)</code> then the original
   * Java <code>Iterator</code> will be returned by the `asJava` method.
   *
   * @param i The <code>Iterator</code> to be converted.
   * @return An object with an `asJava` method that returns a Java <code>Iterator</code> view of the argument.
   */
  implicit def asJavaIteratorConverter[A](i : Iterator[A]): AsJava[ju.Iterator[A]] =
    new AsJava(asJavaIterator(i))

  /**
   * Adds an `asJavaEnumeration` method that implicitly converts a Scala <code>Iterator</code> to a Java <code>Enumeration</code>.
   * The returned Java <code>Enumeration</code> is backed by the provided Scala
   * <code>Iterator</code> and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   * <p>
   * If the Scala <code>Iterator</code> was previously obtained from an implicit or
   * explicit call of <code>asIterator(java.util.Enumeration)</code> then the
   * original Java <code>Enumeration</code> will be returned.
   *
   * @param i The <code>Iterator</code> to be converted.
   * @return An object with an `asJavaEnumeration` method that returns a Java <code>Enumeration</code> view of the argument.
   */
  implicit def asJavaEnumerationConverter[A](i : Iterator[A]): AsJavaEnumeration[A] =
    new AsJavaEnumeration(i)

  /**
   * Adds an `asJava` method that implicitly converts a Scala <code>Iterable</code> to a Java <code>Iterable</code>.
   * The returned Java <code>Iterable</code> is backed by the provided Scala
   * <code>Iterable</code> and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   * <p>
   * If the Scala <code>Iterable</code> was previously obtained from an implicit or
   * explicit call of <code>asIterable(java.lang.Iterable)</code> then the original
   * Java <code>Iterable</code> will be returned.
   *
   * @param i The <code>Iterable</code> to be converted.
   * @return An object with an `asJavaCollection` method that returns a Java <code>Iterable</code> view of the argument.
   */
  implicit def asJavaIterableConverter[A](i : Iterable[A]): AsJava[jl.Iterable[A]] =
    new AsJava(asJavaIterable(i))

  /**
   * Adds an `asJavaCollection` method that implicitly converts a Scala <code>Iterable</code> to an immutable Java
   * <code>Collection</code>.
   * <p>
   * If the Scala <code>Iterable</code> was previously obtained from an implicit or
   * explicit call of <code>asSizedIterable(java.util.Collection)</code> then the original
   * Java <code>Collection</code> will be returned.
   *
   * @param i The <code>SizedIterable</code> to be converted.
   * @return An object with an `asJava` method that returns a Java <code>Collection</code> view of the argument.
   */
  implicit def asJavaCollectionConverter[A](i : Iterable[A]): AsJavaCollection[A] =
    new AsJavaCollection(i)

  /**
   * Adds an `asJava` method that implicitly converts a Scala mutable <code>Buffer</code> to a Java <code>List</code>.
   * The returned Java <code>List</code> is backed by the provided Scala
   * <code>Buffer</code> and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   * <p>
   * If the Scala <code>Buffer</code> was previously obtained from an implicit or
   * explicit call of <code>asBuffer(java.util.List)</code> then the original
   * Java <code>List</code> will be returned.
   *
   * @param b The <code>Buffer</code> to be converted.
   * @return An object with an `asJava` method that returns a Java <code>List</code> view of the argument.
   */
  implicit def bufferAsJavaListConverter[A](b : mutable.Buffer[A]): AsJava[ju.List[A]] =
    new AsJava(bufferAsJavaList(b))

  /**
   * Adds an `asJava` method that implicitly converts a Scala mutable <code>Seq</code> to a Java <code>List</code>.
   * The returned Java <code>List</code> is backed by the provided Scala
   * <code>Seq</code> and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   * <p>
   * If the Scala <code>Seq</code> was previously obtained from an implicit or
   * explicit call of <code>asSeq(java.util.List)</code> then the original
   * Java <code>List</code> will be returned.
   *
   * @param b The <code>Seq</code> to be converted.
   * @return An object with an `asJava` method that returns a Java <code>List</code> view of the argument.
   */
  implicit def mutableSeqAsJavaListConverter[A](b : mutable.Seq[A]): AsJava[ju.List[A]] =
    new AsJava(mutableSeqAsJavaList(b))

  /**
   * Adds an `asJava` method that implicitly converts a Scala <code>Seq</code> to a Java <code>List</code>.
   * The returned Java <code>List</code> is backed by the provided Scala
   * <code>Seq</code> and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   * <p>
   * If the Scala <code>Seq</code> was previously obtained from an implicit or
   * explicit call of <code>asSeq(java.util.List)</code> then the original
   * Java <code>List</code> will be returned.
   *
   * @param b The <code>Seq</code> to be converted.
   * @return An object with an `asJava` method that returns a Java <code>List</code> view of the argument.
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
   * Adds an `asJava` method that implicitly converts a Scala mutable <code>Set</code> to a Java <code>Set</code>.
   * The returned Java <code>Set</code> is backed by the provided Scala
   * <code>Set</code> and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   * <p>
   * If the Scala <code>Set</code> was previously obtained from an implicit or
   * explicit call of <code>asSet(java.util.Set)</code> then the original
   * Java <code>Set</code> will be returned.
   *
   * @param s The <code>Set</code> to be converted.
   * @return An object with an `asJava` method that returns a Java <code>Set</code> view of the argument.
   */
  implicit def mutableSetAsJavaSetConverter[A](s : mutable.Set[A]): AsJava[ju.Set[A]] =
    new AsJava(mutableSetAsJavaSet(s))

  @deprecated("Use mutableSetAsJavaSetConverter instead", "2.9.0")
  def asJavaSetConverter[A](s : mutable.Set[A]): AsJava[ju.Set[A]] = mutableSetAsJavaSetConverter(s)

  /**
   * Adds an `asJava` method that implicitly converts a Scala <code>Set</code> to a Java <code>Set</code>.
   * The returned Java <code>Set</code> is backed by the provided Scala
   * <code>Set</code> and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   * <p>
   * If the Scala <code>Set</code> was previously obtained from an implicit or
   * explicit call of <code>asSet(java.util.Set)</code> then the original
   * Java <code>Set</code> will be returned.
   *
   * @param s The <code>Set</code> to be converted.
   * @return An object with an `asJava` method that returns a Java <code>Set</code> view of the argument.
   */
  implicit def setAsJavaSetConverter[A](s : Set[A]): AsJava[ju.Set[A]] =
    new AsJava(setAsJavaSet(s))

  @deprecated("Use setAsJavaSetConverter instead", "2.9.0")
  def asJavaSetConverter[A](s : Set[A]): AsJava[ju.Set[A]] = setAsJavaSetConverter(s)

  /**
   * Adds an `asJava` method that implicitly converts a Scala mutable <code>Map</code> to a Java <code>Map</code>.
   * The returned Java <code>Map</code> is backed by the provided Scala
   * <code>Map</code> and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   * <p>
   * If the Scala <code>Map</code> was previously obtained from an implicit or
   * explicit call of <code>asMap(java.util.Map)</code> then the original
   * Java <code>Map</code> will be returned.
   *
   * @param m The <code>Map</code> to be converted.
   * @return An object with an `asJava` method that returns a Java <code>Map</code> view of the argument.
   */
  implicit def mutableMapAsJavaMapConverter[A, B](m : mutable.Map[A, B]): AsJava[ju.Map[A, B]] =
    new AsJava(mutableMapAsJavaMap(m))

  @deprecated("use mutableMapAsJavaMapConverter instead", "2.9.0")
  def asJavaMapConverter[A, B](m : mutable.Map[A, B]): AsJava[ju.Map[A, B]] = mutableMapAsJavaMapConverter(m)

  /**
   * Adds an `asJavaDictionary` method that implicitly converts a Scala mutable <code>Map</code> to a Java <code>Dictionary</code>.
   * The returned Java <code>Dictionary</code> is backed by the provided Scala
   * <code>Dictionary</code> and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   * <p>
   * If the Scala <code>Dictionary</code> was previously obtained from an implicit or
   * explicit call of <code>asMap(java.util.Dictionary)</code> then the original
   * Java <code>Dictionary</code> will be returned.
   *
   * @param m The <code>Map</code> to be converted.
   * @return An object with an `asJavaDictionary` method that returns a Java <code>Dictionary</code> view of the argument.
   */
  implicit def asJavaDictionaryConverter[A, B](m : mutable.Map[A, B]): AsJavaDictionary[A, B] =
    new AsJavaDictionary(m)

  /**
   * Adds an `asJava` method that implicitly converts a Scala <code>Map</code> to a Java <code>Map</code>.
   * The returned Java <code>Map</code> is backed by the provided Scala
   * <code>Map</code> and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   * <p>
   * If the Scala <code>Map</code> was previously obtained from an implicit or
   * explicit call of <code>asMap(java.util.Map)</code> then the original
   * Java <code>Map</code> will be returned.
   *
   * @param m The <code>Map</code> to be converted.
   * @return An object with an `asJava` method that returns a Java <code>Map</code> view of the argument.
   */
  implicit def mapAsJavaMapConverter[A, B](m : Map[A, B]): AsJava[ju.Map[A, B]] =
    new AsJava(mapAsJavaMap(m))

  @deprecated("Use mapAsJavaMapConverter instead", "2.9.0")
  def asJavaMapConverter[A, B](m : Map[A, B]): AsJava[ju.Map[A, B]] = mapAsJavaMapConverter(m)

  /**
   * Adds an `asJava` method that implicitly converts a Scala mutable `ConcurrentMap` to a Java `ConcurrentMap`.
   * The returned Java `ConcurrentMap` is backed by the provided Scala `ConcurrentMap`
   * and any side-effects of using it via the Java interface will be visible
   * via the Scala interface and vice versa.
   * <p>
   * If the Scala <code>ConcurrentMap</code> was previously obtained from an implicit or
   * explicit call of <code>asConcurrentMap(java.util.concurrect.ConcurrentMap)</code> then the original
   * Java <code>ConcurrentMap</code> will be returned.
   *
   * @param m The <code>ConcurrentMap</code> to be converted.
   * @return An object with an `asJava` method that returns a Java <code>ConcurrentMap</code> view of the argument.
   */
  implicit def asJavaConcurrentMapConverter[A, B](m: mutable.ConcurrentMap[A, B]): AsJava[juc.ConcurrentMap[A, B]] =
    new AsJava(asJavaConcurrentMap(m))

  /**
   * Adds an `asScala` method that implicitly converts a Java <code>Iterator</code> to a Scala <code>Iterator</code>.
   * The returned Scala <code>Iterator</code> is backed by the provided Java
   * <code>Iterator</code> and any side-effects of using it via the Scala interface will
   * be visible via the Java interface and vice versa.
   * <p>
   * If the Java <code>Iterator</code> was previously obtained from an implicit or
   * explicit call of <code>asIterator(scala.collection.Iterator)</code> then the original
   * Scala <code>Iterator</code> will be returned.
   *
   * @param i The <code>Iterator</code> to be converted.
   * @return An object with an `asScala` method that returns a Scala <code>Iterator</code> view of the argument.
   */
  implicit def asScalaIteratorConverter[A](i : ju.Iterator[A]): AsScala[Iterator[A]] =
    new AsScala(asScalaIterator(i))

  /**
   * Adds an `asScala` method that implicitly converts a Java <code>Enumeration</code> to a Scala <code>Iterator</code>.
   * The returned Scala <code>Iterator</code> is backed by the provided Java
   * <code>Enumeration</code> and any side-effects of using it via the Scala interface will
   * be visible via the Java interface and vice versa.
   * <p>
   * If the Java <code>Enumeration</code> was previously obtained from an implicit or
   * explicit call of <code>asEnumeration(scala.collection.Iterator)</code> then the
   * original Scala <code>Iterator</code> will be returned.
   *
   * @param i The <code>Enumeration</code> to be converted.
   * @return An object with an `asScala` method that returns a Scala <code>Iterator</code> view of the argument.
   */
  implicit def enumerationAsScalaIteratorConverter[A](i : ju.Enumeration[A]): AsScala[Iterator[A]] =
    new AsScala(enumerationAsScalaIterator(i))

  /**
   * Adds an `asScala` method that implicitly converts a Java <code>Iterable</code> to a Scala <code>Iterable</code>.
   * The returned Scala <code>Iterable</code> is backed by the provided Java
   * <code>Iterable</code> and any side-effects of using it via the Scala interface will
   * be visible via the Java interface and vice versa.
   * <p>
   * If the Java <code>Iterable</code> was previously obtained from an implicit or
   * explicit call of <code>asIterable(scala.collection.Iterable)</code> then the original
   * Scala <code>Iterable</code> will be returned.
   *
   * @param i The <code>Iterable</code> to be converted.
   * @return An object with an `asScala` method that returns a Scala <code>Iterable</code> view of the argument.
   */
  implicit def iterableAsScalaIterableConverter[A](i : jl.Iterable[A]): AsScala[Iterable[A]] =
    new AsScala(iterableAsScalaIterable(i))

  @deprecated("Use iterableAsScalaIterableConverter instead", "2.9.0")
  def asScalaIterableConverter[A](i : jl.Iterable[A]): AsScala[Iterable[A]] = iterableAsScalaIterableConverter(i)

  /**
   * Adds an `asScala` method that implicitly converts a Java <code>Collection</code> to an Scala <code>Iterable</code>.
   * <p>
   * If the Java <code>Collection</code> was previously obtained from an implicit or
   * explicit call of <code>asCollection(scala.collection.SizedIterable)</code> then
   * the original Scala <code>SizedIterable</code> will be returned.
   *
   * @param i The <code>Collection</code> to be converted.
   * @return An object with an `asScala` method that returns a Scala <code>SizedIterable</code> view of the argument.
   */
  implicit def collectionAsScalaIterableConverter[A](i : ju.Collection[A]): AsScala[Iterable[A]] =
    new AsScala(collectionAsScalaIterable(i))

  @deprecated("Use collectionAsScalaIterableConverter instead", "2.9.0")
  def asScalaIterableConverter[A](i : ju.Collection[A]): AsScala[Iterable[A]] = collectionAsScalaIterableConverter(i)

  /**
   * Adds an `asScala` method that implicitly converts a Java <code>List</code> to a Scala mutable <code>Buffer</code>.
   * The returned Scala <code>Buffer</code> is backed by the provided Java
   * <code>List</code> and any side-effects of using it via the Scala interface will
   * be visible via the Java interface and vice versa.
   * <p>
   * If the Java <code>List</code> was previously obtained from an implicit or
   * explicit call of <code>asList(scala.collection.mutable.Buffer)</code> then the original
   * Scala <code>Buffer</code> will be returned.
   *
   * @param l The <code>List</code> to be converted.
   * @return An object with an `asScala` method that returns a Scala mutable <code>Buffer</code> view of the argument.
   */
  implicit def asScalaBufferConverter[A](l : ju.List[A]): AsScala[mutable.Buffer[A]] =
    new AsScala(asScalaBuffer(l))

  /**
   * Adds an `asScala` method that implicitly converts a Java <code>Set</code> to a Scala mutable <code>Set</code>.
   * The returned Scala <code>Set</code> is backed by the provided Java
   * <code>Set</code> and any side-effects of using it via the Scala interface will
   * be visible via the Java interface and vice versa.
   * <p>
   * If the Java <code>Set</code> was previously obtained from an implicit or
   * explicit call of <code>asSet(scala.collection.mutable.Set)</code> then the original
   * Scala <code>Set</code> will be returned.
   *
   * @param s The <code>Set</code> to be converted.
   * @return An object with an `asScala` method that returns a Scala mutable <code>Set</code> view of the argument.
   */
  implicit def asScalaSetConverter[A](s : ju.Set[A]): AsScala[mutable.Set[A]] =
    new AsScala(asScalaSet(s))

  /**
   * Adds an `asScala` method that implicitly converts a Java <code>Map</code> to a Scala mutable <code>Map</code>.
   * The returned Scala <code>Map</code> is backed by the provided Java
   * <code>Map</code> and any side-effects of using it via the Scala interface will
   * be visible via the Java interface and vice versa.
   * <p>
   * If the Java <code>Map</code> was previously obtained from an implicit or
   * explicit call of <code>asMap(scala.collection.mutable.Map)</code> then the original
   * Scala <code>Map</code> will be returned.
   *
   * @param m The <code>Map</code> to be converted.
   * @return An object with an `asScala` method that returns a Scala mutable <code>Map</code> view of the argument.
   */
  implicit def mapAsScalaMapConverter[A, B](m : ju.Map[A, B]): AsScala[mutable.Map[A, B]] =
    new AsScala(mapAsScalaMap(m))

  @deprecated("Use mapAsScalaMapConverter instead", "2.9.0")
  def asScalaMapConverter[A, B](m : ju.Map[A, B]): AsScala[mutable.Map[A, B]] = mapAsScalaMapConverter(m)

  /**
   * Adds an `asScala` method that implicitly converts a Java <code>ConcurrentMap</code> to a Scala mutable <code>ConcurrentMap</code>.
   * The returned Scala <code>ConcurrentMap</code> is backed by the provided Java
   * <code>ConcurrentMap</code> and any side-effects of using it via the Scala interface will
   * be visible via the Java interface and vice versa.
   * <p>
   * If the Java <code>ConcurrentMap</code> was previously obtained from an implicit or
   * explicit call of <code>asConcurrentMap(scala.collection.mutable.ConcurrentMap)</code> then the original
   * Scala <code>ConcurrentMap</code> will be returned.
   *
   * @param m The <code>ConcurrentMap</code> to be converted.
   * @return An object with an `asScala` method that returns a Scala mutable <code>ConcurrentMap</code> view of the argument.
   */
  implicit def asScalaConcurrentMapConverter[A, B](m: juc.ConcurrentMap[A, B]): AsScala[mutable.ConcurrentMap[A, B]] =
    new AsScala(asScalaConcurrentMap(m))

  /**
   * Adds an `asScala` method that implicitly converts a Java <code>Dictionary</code> to a Scala mutable <code>Map[String, String]</code>.
   * The returned Scala <code>Map[String, String]</code> is backed by the provided Java
   * <code>Dictionary</code> and any side-effects of using it via the Scala interface will
   * be visible via the Java interface and vice versa.
   *
   * @param m The <code>Dictionary</code> to be converted.
   * @return An object with an `asScala` method that returns a Scala mutable <code>Map[String, String]</code> view of the argument.
   */
  implicit def dictionaryAsScalaMapConverter[A, B](p: ju.Dictionary[A, B]): AsScala[mutable.Map[A, B]] =
    new AsScala(dictionaryAsScalaMap(p))

  /**
   * Adds an `asScala` method that implicitly converts a Java <code>Properties</code> to a Scala mutable <code>Map[String, String]</code>.
   * The returned Scala <code>Map[String, String]</code> is backed by the provided Java
   * <code>Properties</code> and any side-effects of using it via the Scala interface will
   * be visible via the Java interface and vice versa.
   *
   * @param m The <code>Properties</code> to be converted.
   * @return An object with an `asScala` method that returns a Scala mutable <code>Map[String, String]</code> view of the argument.
   */
  implicit def propertiesAsScalaMapConverter(p: ju.Properties): AsScala[mutable.Map[String, String]] =
    new AsScala(propertiesAsScalaMap(p))

  @deprecated("Use propertiesAsScalaMapConverter instead", "2.9.0")
  def asScalaMapConverter(p: ju.Properties): AsScala[mutable.Map[String, String]] =
    propertiesAsScalaMapConverter(p)

}
