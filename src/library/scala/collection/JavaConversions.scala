/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection

/** <p>
 *    A collection of implicit conversions supporting interoperability between
 *    Scala and Java collections.
 *  </p>
 *  <p>
 *    The following conversions are supported:
 *  </p>
 *  <ul>
 *    <li><code>scala.collection.Iterable</code> <=> <code>java.lang.Iterable</code></li>
 *    <li><code>scala.collection.Iterable</code> <=> <code>java.util.Collection</code></li>
 *    <li><code>scala.collection.Iterator</code> <=> <code>java.util.{ Iterator, Enumeration }</code></li>
 *    <li><code>scala.collection.mutable.Buffer</code> <=> <code>java.util.List</code></li>
 *    <li><code>scala.collection.mutable.Set</code> <=> <code>java.util.Set</code></li>
 *    <li><code>scala.collection.mutable.Map</code> <=> <code>java.util.{ Map, Dictionary }</code></li>
 *    <li><code>scala.collection.mutable.ConcurrentMap</code> <=> <code>java.util.concurrent.ConcurrentMap</code></li>
 *  </ul>
 *  <p>
 *    In all cases, converting from a source type to a target type and back
 *    again will return the original source object, eg.
 *  </p>
 *  <pre>
 *    <b>import</b> scala.collection.JavaConversions._
 *
 *    <b>val</b> sl = <b>new</b> scala.collection.mutable.ListBuffer[Int]
 *    <b>val</b> jl : java.util.List[Int] = sl
 *    <b>val</b> sl2 : scala.collection.mutable.Buffer[Int] = jl
 *    assert(sl eq sl2)g</pre>
 *  <p>
 *  In addition, the following one way conversions are provided:
 *  </p>
 *  <ul>
 *    <li><code>scala.collection.Seq => <code>java.util.List }</code></li>
 *    <li><code>scala.collection.mutable.Seq => <code>java.util.List</code></li>
 *    <li><code>scala.collection.Set</code> => <code>java.util.Set</code></li>
 *    <li><code>scala.collection.Map</code> => <code>java.util.Map</code></li>
 *  </ul>
 *
 *  @author Miles Sabin
 *  @author Martin Odersky
 *  @since  2.8
 */
object JavaConversions {
  import java.{ lang => jl, util => ju }
  import java.util.{ concurrent => juc }

  // Scala => Java

  /**
   * Implicitly converts a Scala <code>Iterator</code> to a Java <code>Iterator</code>.
   * The returned Java <code>Iterator</code> is backed by the provided Scala
   * <code>Iterator</code> and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   * <p>
   * If the Scala <code>Iterator</code> was previously obtained from an implicit or
   * explicit call of <code>asIterator(java.util.Iterator)</code> then the original
   * Java <code>Iterator</code> will be returned.
   *
   * @param i The <code>Iterator</code> to be converted.
   * @return A Java <code>Iterator</code> view of the argument.
   */
  implicit def asJavaIterator[A](i : Iterator[A]): ju.Iterator[A] = i match {
    case JIteratorWrapper(wrapped) => wrapped
    case _ => IteratorWrapper(i)
  }

  @deprecated("use asJavaIterator instead")
  def asIterator[A](i : Iterator[A]): ju.Iterator[A] = asJavaIterator[A](i)

  /**
   * Implicitly converts a Scala <code>Iterator</code> to a Java <code>Enumeration</code>.
   * The returned Java <code>Enumeration</code> is backed by the provided Scala
   * <code>Iterator</code> and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   * <p>
   * If the Scala <code>Iterator</code> was previously obtained from an implicit or
   * explicit call of <code>asIterator(java.util.Enumeration)</code> then the
   * original Java <code>Enumeration</code> will be returned.
   *
   * @param i The <code>Iterator</code> to be converted.
   * @return A Java <code>Enumeration</code> view of the argument.
   */
  implicit def asJavaEnumeration[A](i : Iterator[A]): ju.Enumeration[A] = i match {
    case JEnumerationWrapper(wrapped) => wrapped
    case _ => IteratorWrapper(i)
  }

  @deprecated("use asJavaEnmeration instead")
  def asEnumeration[A](i : Iterator[A]): ju.Enumeration[A] = asJavaEnumeration[A](i)

  /**
   * Implicitly converts a Scala <code>Iterable</code> to a Java <code>Iterable</code>.
   * The returned Java <code>Iterable</code> is backed by the provided Scala
   * <code>Iterable</code> and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   * <p>
   * If the Scala <code>Iterable</code> was previously obtained from an implicit or
   * explicit call of <code>asIterable(java.lang.Iterable)</code> then the original
   * Java <code>Iterable</code> will be returned.
   *
   * @param i The <code>Iterable</code> to be converted.
   * @return A Java <code>Iterable</code> view of the argument.
   */
  implicit def asJavaIterable[A](i : Iterable[A]): jl.Iterable[A] = i match {
    case JIterableWrapper(wrapped) => wrapped
    case _ => IterableWrapper(i)
  }

  @deprecated("use asJavaIterable instead")
  def asIterable[A](i : Iterable[A]): jl.Iterable[A] = asJavaIterable[A](i)

  /**
   * Implicitly converts a Scala <code>Iterable</code> to an immutable Java
   * <code>Collection</code>.
   * <p>
   * If the Scala <code>Iterable</code> was previously obtained from an implicit or
   * explicit call of <code>asSizedIterable(java.util.Collection)</code> then the original
   * Java <code>Collection</code> will be returned.
   *
   * @param i The <code>SizedIterable</code> to be converted.
   * @return A Java <code>Collection</code> view of the argument.
   */
  implicit def asJavaCollection[A](i : Iterable[A]): ju.Collection[A] = i match {
    case JCollectionWrapper(wrapped) => wrapped
    case _ => new IterableWrapper(i)
  }

  @deprecated("use asJavaCollection instead")
  def asCollection[A](i : Iterable[A]): ju.Collection[A] = asJavaCollection[A](i)

  /**
   * Implicitly converts a Scala mutable <code>Buffer</code> to a Java <code>List</code>.
   * The returned Java <code>List</code> is backed by the provided Scala
   * <code>Buffer</code> and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   * <p>
   * If the Scala <code>Buffer</code> was previously obtained from an implicit or
   * explicit call of <code>asBuffer(java.util.List)</code> then the original
   * Java <code>List</code> will be returned.
   *
   * @param b The <code>Buffer</code> to be converted.
   * @return A Java <code>List</code> view of the argument.
   */
  implicit def asJavaList[A](b : mutable.Buffer[A]): ju.List[A] = b match {
    case JListWrapper(wrapped) => wrapped
    case _ => new MutableBufferWrapper(b)
  }

  @deprecated("use asJavaList instead")
  def asList[A](b : mutable.Buffer[A]): ju.List[A] = asJavaList[A](b)

  /**
   * Implicitly converts a Scala mutable <code>Seq</code> to a Java <code>List</code>.
   * The returned Java <code>List</code> is backed by the provided Scala
   * <code>Seq</code> and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   * <p>
   * If the Scala <code>Seq</code> was previously obtained from an implicit or
   * explicit call of <code>asSeq(java.util.List)</code> then the original
   * Java <code>List</code> will be returned.
   *
   * @param b The <code>Seq</code> to be converted.
   * @return A Java <code>List</code> view of the argument.
   */
  implicit def asJavaList[A](b : mutable.Seq[A]): ju.List[A] = b match {
    case JListWrapper(wrapped) => wrapped
    case _ => new MutableSeqWrapper(b)
  }

  @deprecated("use asJavaList instead")
  def asList[A](b : mutable.Seq[A]): ju.List[A] = asJavaList[A](b)

  /**
   * Implicitly converts a Scala <code>Seq</code> to a Java <code>List</code>.
   * The returned Java <code>List</code> is backed by the provided Scala
   * <code>Seq</code> and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   * <p>
   * If the Scala <code>Seq</code> was previously obtained from an implicit or
   * explicit call of <code>asSeq(java.util.List)</code> then the original
   * Java <code>List</code> will be returned.
   *
   * @param b The <code>Seq</code> to be converted.
   * @return A Java <code>List</code> view of the argument.
   */
  implicit def asJavaList[A](b : Seq[A]): ju.List[A] = b match {
    case JListWrapper(wrapped) => wrapped
    case _ => new SeqWrapper(b)
  }

  @deprecated("use asJavaList instead")
  def asList[A](b : Seq[A]): ju.List[A] = asJavaList[A](b)

  /**
   * Implicitly converts a Scala mutable <code>Set</code> to a Java <code>Set</code>.
   * The returned Java <code>Set</code> is backed by the provided Scala
   * <code>Set</code> and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   * <p>
   * If the Scala <code>Set</code> was previously obtained from an implicit or
   * explicit call of <code>asSet(java.util.Set)</code> then the original
   * Java <code>Set</code> will be returned.
   *
   * @param s The <code>Set</code> to be converted.
   * @return A Java <code>Set</code> view of the argument.
   */
  implicit def asJavaSet[A](s : mutable.Set[A]): ju.Set[A] = s match {
    case JSetWrapper(wrapped) => wrapped
    case _ => new MutableSetWrapper(s)
  }

  @deprecated("use asJavaSet instead")
  def asSet[A](s : mutable.Set[A]): ju.Set[A] = asJavaSet[A](s)

  /**
   * Implicitly converts a Scala <code>Set</code> to a Java <code>Set</code>.
   * The returned Java <code>Set</code> is backed by the provided Scala
   * <code>Set</code> and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   * <p>
   * If the Scala <code>Set</code> was previously obtained from an implicit or
   * explicit call of <code>asSet(java.util.Set)</code> then the original
   * Java <code>Set</code> will be returned.
   *
   * @param s The <code>Set</code> to be converted.
   * @return A Java <code>Set</code> view of the argument.
   */
  implicit def asJavaSet[A](s: Set[A]): ju.Set[A] = s match {
    case JSetWrapper(wrapped) => wrapped
    case _ => new SetWrapper(s)
  }

  @deprecated("use asJavaSet instead")
  def asSet[A](s : Set[A]): ju.Set[A] = asJavaSet[A](s)

  /**
   * Implicitly converts a Scala mutable <code>Map</code> to a Java <code>Map</code>.
   * The returned Java <code>Map</code> is backed by the provided Scala
   * <code>Map</code> and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   * <p>
   * If the Scala <code>Map</code> was previously obtained from an implicit or
   * explicit call of <code>asMap(java.util.Map)</code> then the original
   * Java <code>Map</code> will be returned.
   *
   * @param m The <code>Map</code> to be converted.
   * @return A Java <code>Map</code> view of the argument.
   */
  implicit def asJavaMap[A, B](m : mutable.Map[A, B]): ju.Map[A, B] = m match {
    //case JConcurrentMapWrapper(wrapped) => wrapped
    case JMapWrapper(wrapped) => wrapped
    case _ => new MutableMapWrapper(m)
  }

  @deprecated("use asJavaMap instead")
  def asMap[A, B](m : mutable.Map[A, B]): ju.Map[A, B] = asJavaMap[A, B](m)

  /**
   * Implicitly converts a Scala mutable <code>Map</code> to a Java <code>Dictionary</code>.
   * The returned Java <code>Dictionary</code> is backed by the provided Scala
   * <code>Dictionary</code> and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   * <p>
   * If the Scala <code>Dictionary</code> was previously obtained from an implicit or
   * explicit call of <code>asMap(java.util.Dictionary)</code> then the original
   * Java <code>Dictionary</code> will be returned.
   *
   * @param m The <code>Map</code> to be converted.
   * @return A Java <code>Dictionary</code> view of the argument.
   */
  implicit def asJavaDictionary[A, B](m : mutable.Map[A, B]): ju.Dictionary[A, B] = m match {
    //case JConcurrentMapWrapper(wrapped) => wrapped
    case JDictionaryWrapper(wrapped) => wrapped
    case _ => new DictionaryWrapper(m)
  }

  @deprecated("use asJavaDictionary instead")
  def asDictionary[A, B](m : mutable.Map[A, B]): ju.Dictionary[A, B] = asJavaDictionary[A, B](m)

  /**
   * Implicitly converts a Scala <code>Map</code> to a Java <code>Map</code>.
   * The returned Java <code>Map</code> is backed by the provided Scala
   * <code>Map</code> and any side-effects of using it via the Java interface will
   * be visible via the Scala interface and vice versa.
   * <p>
   * If the Scala <code>Map</code> was previously obtained from an implicit or
   * explicit call of <code>asMap(java.util.Map)</code> then the original
   * Java <code>Map</code> will be returned.
   *
   * @param m The <code>Map</code> to be converted.
   * @return A Java <code>Map</code> view of the argument.
   */
  implicit def asJavaMap[A, B](m : Map[A, B]): ju.Map[A, B] = m match {
    //case JConcurrentMapWrapper(wrapped) => wrapped
    case JMapWrapper(wrapped) => wrapped
    case _ => new MapWrapper(m)
  }

  @deprecated("use asJavaMap instead")
  def asMap[A, B](m : Map[A, B]): ju.Map[A, B] = asJavaMap[A, B](m)

  /**
   * Implicitly converts a Scala mutable `ConcurrentMap` to a Java `ConcurrentMap`.
   * The returned Java `ConcurrentMap` is backed by the provided Scala `ConcurrentMap`
   * and any side-effects of using it via the Java interface will be visible
   * via the Scala interface and vice versa.
   * <p>
   * If the Scala <code>ConcurrentMap</code> was previously obtained from an implicit or
   * explicit call of <code>asConcurrentMap(java.util.concurrect.ConcurrentMap)</code> then the original
   * Java <code>ConcurrentMap</code> will be returned.
   *
   * @param m The <code>ConcurrentMap</code> to be converted.
   * @return A Java <code>ConcurrentMap</code> view of the argument.
   */
  implicit def asJavaConcurrentMap[A, B](m: mutable.ConcurrentMap[A, B]): juc.ConcurrentMap[A, B] = m match {
    case JConcurrentMapWrapper(wrapped) => wrapped
    case _ => new ConcurrentMapWrapper(m)
  }

  @deprecated("use asJavaConcurrentMap instead")
  def asConcurrentMap[A, B](m: mutable.ConcurrentMap[A, B]): juc.ConcurrentMap[A, B] = asJavaConcurrentMap[A, B](m)

  // Java => Scala

  /**
   * Implicitly converts a Java <code>Iterator</code> to a Scala <code>Iterator</code>.
   * The returned Scala <code>Iterator</code> is backed by the provided Java
   * <code>Iterator</code> and any side-effects of using it via the Scala interface will
   * be visible via the Java interface and vice versa.
   * <p>
   * If the Java <code>Iterator</code> was previously obtained from an implicit or
   * explicit call of <code>asIterator(scala.collection.Iterator)</code> then the original
   * Scala <code>Iterator</code> will be returned.
   *
   * @param i The <code>Iterator</code> to be converted.
   * @return A Scala <code>Iterator</code> view of the argument.
   */
  implicit def asScalaIterator[A](i : ju.Iterator[A]): Iterator[A] = i match {
    case IteratorWrapper(wrapped) => wrapped
    case _ => JIteratorWrapper(i)
  }

  @deprecated("use asScalaIterator instead")
  def asIterator[A](i : ju.Iterator[A]): Iterator[A] = asScalaIterator[A](i)

  /**
   * Implicitly converts a Java <code>Enumeration</code> to a Scala <code>Iterator</code>.
   * The returned Scala <code>Iterator</code> is backed by the provided Java
   * <code>Enumeration</code> and any side-effects of using it via the Scala interface will
   * be visible via the Java interface and vice versa.
   * <p>
   * If the Java <code>Enumeration</code> was previously obtained from an implicit or
   * explicit call of <code>asEnumeration(scala.collection.Iterator)</code> then the
   * original Scala <code>Iterator</code> will be returned.
   *
   * @param i The <code>Enumeration</code> to be converted.
   * @return A Scala <code>Iterator</code> view of the argument.
   */
  implicit def enumerationAsScalaIterator[A](i : ju.Enumeration[A]): Iterator[A] = i match {
    case IteratorWrapper(wrapped) => wrapped
    case _ => JEnumerationWrapper(i)
  }

  @deprecated("use enumerationAsScalaIterator instead")
  def asIterator[A](i : ju.Enumeration[A]): Iterator[A] = enumerationAsScalaIterator[A](i)

  /**
   * Implicitly converts a Java <code>Iterable</code> to a Scala <code>Iterable</code>.
   * The returned Scala <code>Iterable</code> is backed by the provided Java
   * <code>Iterable</code> and any side-effects of using it via the Scala interface will
   * be visible via the Java interface and vice versa.
   * <p>
   * If the Java <code>Iterable</code> was previously obtained from an implicit or
   * explicit call of <code>asIterable(scala.collection.Iterable)</code> then the original
   * Scala <code>Iterable</code> will be returned.
   *
   * @param i The <code>Iterable</code> to be converted.
   * @return A Scala <code>Iterable</code> view of the argument.
   */
  implicit def asScalaIterable[A](i : jl.Iterable[A]): Iterable[A] = i match {
    case IterableWrapper(wrapped) => wrapped
    case _ => JIterableWrapper(i)
  }

  @deprecated("use asScalaIterable instead")
  def asIterable[A](i : jl.Iterable[A]): Iterable[A] = asScalaIterable[A](i)

  /**
   * Implicitly converts a Java <code>Collection</code> to an Scala <code>Iterable</code>.
   * <p>
   * If the Java <code>Collection</code> was previously obtained from an implicit or
   * explicit call of <code>asCollection(scala.collection.SizedIterable)</code> then
   * the original Scala <code>SizedIterable</code> will be returned.
   *
   * @param i The <code>Collection</code> to be converted.
   * @return A Scala <code>SizedIterable</code> view of the argument.
   */
  implicit def asScalaIterable[A](i : ju.Collection[A]): Iterable[A] = i match {
    case IterableWrapper(wrapped) => wrapped
    case _ => JCollectionWrapper(i)
  }

  @deprecated("use asScalaIterable instead")
  def asIterable[A](i : ju.Collection[A]): Iterable[A] = asScalaIterable[A](i)

  /**
   * Implicitly converts a Java <code>List</code> to a Scala mutable <code>Buffer</code>.
   * The returned Scala <code>Buffer</code> is backed by the provided Java
   * <code>List</code> and any side-effects of using it via the Scala interface will
   * be visible via the Java interface and vice versa.
   * <p>
   * If the Java <code>List</code> was previously obtained from an implicit or
   * explicit call of <code>asList(scala.collection.mutable.Buffer)</code> then the original
   * Scala <code>Buffer</code> will be returned.
   *
   * @param l The <code>List</code> to be converted.
   * @return A Scala mutable <code>Buffer</code> view of the argument.
   */
  implicit def asScalaBuffer[A](l : ju.List[A]): mutable.Buffer[A] = l match {
    case MutableBufferWrapper(wrapped) => wrapped
    case _ =>new JListWrapper(l)
  }

  @deprecated("use asScalaBuffer instead")
  def asBuffer[A](l : ju.List[A]): mutable.Buffer[A] = asScalaBuffer[A](l)

  /**
   * Implicitly converts a Java <code>Set</code> to a Scala mutable <code>Set</code>.
   * The returned Scala <code>Set</code> is backed by the provided Java
   * <code>Set</code> and any side-effects of using it via the Scala interface will
   * be visible via the Java interface and vice versa.
   * <p>
   * If the Java <code>Set</code> was previously obtained from an implicit or
   * explicit call of <code>asSet(scala.collection.mutable.Set)</code> then the original
   * ScalaThe reported problems have to do with dependent method types, which is currently an experimental feature in Scala and is still under development. We emphasize that these problems are related to type-inference and, as stated in the paper, it is possible to run and type-check the programs with additional annotations. <code>Set</code> will be returned.
   *
   * @param s The <code>Set</code> to be converted.
   * @return A Scala mutable <code>Set</code> view of the argument.
   */
  implicit def asScalaSet[A](s : ju.Set[A]): mutable.Set[A] = s match {
    case MutableSetWrapper(wrapped) => wrapped
    case _ =>new JSetWrapper(s)
  }

  @deprecated("use asScalaSet instead")
  def asSet[A](s : ju.Set[A]): mutable.Set[A] = asScalaSet[A](s)

  /**
   * Implicitly converts a Java <code>Map</code> to a Scala mutable <code>Map</code>.
   * The returned Scala <code>Map</code> is backed by the provided Java
   * <code>Map</code> and any side-effects of using it via the Scala interface will
   * be visible via the Java interface and vice versa.
   * <p>
   * If the Java <code>Map</code> was previously obtained from an implicit or
   * explicit call of <code>asMap(scala.collection.mutable.Map)</code> then the original
   * Scala <code>Map</code> will be returned.
   *
   * @param m The <code>Map</code> to be converted.
   * @return A Scala mutable <code>Map</code> view of the argument.
   */
  implicit def asScalaMap[A, B](m : ju.Map[A, B]): mutable.Map[A, B] = m match {
    //case ConcurrentMapWrapper(wrapped) => wrapped
    case MutableMapWrapper(wrapped) => wrapped
    case _ => new JMapWrapper(m)
  }

  @deprecated("use asScalaMap instead")
  def asMap[A, B](m : ju.Map[A, B]): mutable.Map[A, B] = asScalaMap[A, B](m)

  /**
   * Implicitly converts a Java <code>ConcurrentMap</code> to a Scala mutable <code>ConcurrentMap</code>.
   * The returned Scala <code>ConcurrentMap</code> is backed by the provided Java
   * <code>ConcurrentMap</code> and any side-effects of using it via the Scala interface will
   * be visible via the Java interface and vice versa.
   * <p>
   * If the Java <code>ConcurrentMap</code> was previously obtained from an implicit or
   * explicit call of <code>asConcurrentMap(scala.collection.mutable.ConcurrentMap)</code> then the original
   * Scala <code>ConcurrentMap</code> will be returned.
   *
   * @param m The <code>ConcurrentMap</code> to be converted.
   * @return A Scala mutable <code>ConcurrrentMap</code> view of the argument.
   */
  implicit def asScalaConcurrentMap[A, B](m: juc.ConcurrentMap[A, B]): mutable.ConcurrentMap[A, B] = m match {
    case cmw: ConcurrentMapWrapper[a, b] => cmw.underlying
    case _ => new JConcurrentMapWrapper(m)
  }

  @deprecated("use asScalaConcurrentMap instead")
  def asConcurrentMap[A, B](m: juc.ConcurrentMap[A, B]): mutable.ConcurrentMap[A, B] = asScalaConcurrentMap[A, B](m)

  /**
   * Implicitly converts a Java <code>Dictionary</code> to a Scala mutable <code>Map[String, String]</code>.
   * The returned Scala <code>Map[String, String]</code> is backed by the provided Java
   * <code>Dictionary</code> and any side-effects of using it via the Scala interface will
   * be visible via the Java interface and vice versa.
   *
   * @param m The <code>Dictionary</code> to be converted.
   * @return A Scala mutable <code>Map[String, String]</code> view of the argument.
   */
  implicit def dictionaryAsScalaMap[A, B](p: ju.Dictionary[A, B]): mutable.Map[A, B] = p match {
    case DictionaryWrapper(wrapped) => wrapped
    case _ => new JDictionaryWrapper(p)
  }

  @deprecated("use dictionaryAsScalaMap instead")
  def asMap[A, B](p: ju.Dictionary[A, B]): mutable.Map[A, B] = dictionaryAsScalaMap[A, B](p)

  /**
   * Implicitly converts a Java <code>Properties</code> to a Scala mutable <code>Map[String, String]</code>.
   * The returned Scala <code>Map[String, String]</code> is backed by the provided Java
   * <code>Properties</code> and any side-effects of using it via the Scala interface will
   * be visible via the Java interface and vice versa.
   *
   * @param m The <code>Properties</code> to be converted.
   * @return A Scala mutable <code>Map[String, String]</code> view of the argument.
   */
  implicit def asScalaMap(p: ju.Properties): mutable.Map[String, String] = p match {
    case _ => new JPropertiesWrapper(p)
  }

  @deprecated("use asScalaMap instead")
  def asMap(p: ju.Properties): mutable.Map[String, String] = asScalaMap(p)

  // Private implementations (shared by JavaConverters) ...

  case class IteratorWrapper[A](underlying : Iterator[A]) extends ju.Iterator[A] with ju.Enumeration[A] {
    def hasNext = underlying.hasNext
    def next = underlying.next
    def hasMoreElements = underlying.hasNext
    def nextElement = underlying.next
    def remove = throw new UnsupportedOperationException
  }

  class ToIteratorWrapper[A](underlying : Iterator[A]) {
    def asJava = new IteratorWrapper(underlying)
  }

  case class JIteratorWrapper[A](underlying : ju.Iterator[A]) extends Iterator[A] {
    def hasNext = underlying.hasNext
    def next = underlying.next
  }

  case class JEnumerationWrapper[A](underlying : ju.Enumeration[A]) extends Iterator[A] {
    def hasNext = underlying.hasMoreElements
    def next = underlying.nextElement
  }

  case class IterableWrapper[A](underlying : Iterable[A]) extends ju.AbstractCollection[A] {
    def iterator = underlying.iterator
    def size = underlying.size
    override def isEmpty = underlying.isEmpty
  }

  case class JIterableWrapper[A](underlying : jl.Iterable[A]) extends Iterable[A] {
    def iterator = underlying.iterator
    def newBuilder[B] = new mutable.ArrayBuffer[B]
  }

  case class JCollectionWrapper[A](underlying : ju.Collection[A]) extends Iterable[A] {
    def iterator = underlying.iterator
    override def size = underlying.size
    override def isEmpty = underlying.isEmpty
    def newBuilder[B] = new mutable.ArrayBuffer[B]
  }

  case class SeqWrapper[A](underlying : Seq[A]) extends ju.AbstractList[A] {
    def size = underlying.length
    def get(i : Int) = underlying(i)
  }

  case class MutableSeqWrapper[A](underlying : mutable.Seq[A]) extends ju.AbstractList[A] {
    def size = underlying.length
    def get(i : Int) = underlying(i)
    override def set(i : Int, elem: A) = { val p = underlying(i) ; underlying(i) = elem ; p }
  }

  case class MutableBufferWrapper[A](underlying : mutable.Buffer[A]) extends ju.AbstractList[A] {
    def size = underlying.length
    def get(i : Int) = underlying(i)
    override def set(i : Int, elem: A) = { val p = underlying(i) ; underlying(i) = elem ; p }
    override def add(elem : A) = { underlying.append(elem) ; true }
    override def remove(i : Int) = underlying.remove(i)
  }

  case class JListWrapper[A](val underlying : ju.List[A]) extends mutable.Buffer[A] {
    def length = underlying.size
    override def isEmpty = underlying.isEmpty
    override def iterator : Iterator[A] = underlying.iterator
    def apply(i : Int) = underlying.get(i)
    def update(i : Int, elem : A) = underlying.set(i, elem)
    def +=:(elem : A) = { underlying.subList(0, 0).add(elem) ; this }
    def +=(elem : A): this.type = { underlying.add(elem); this }
    def insertAll(i : Int, elems : Traversable[A]) = { val ins = underlying.subList(0, i) ;  elems.foreach(ins.add(_)) }
    def remove(i : Int) = underlying.remove(i)
    def clear = underlying.clear
    def result = this
  }

  class SetWrapper[A](underlying: Set[A]) extends ju.AbstractSet[A] {
    self =>
    def size = underlying.size
    def iterator = new ju.Iterator[A] {
      val ui = underlying.iterator
      var prev : Option[A] = None
      def hasNext = ui.hasNext
      def next = { val e = ui.next ; prev = Some(e) ; e }
      def remove = prev match {
        case Some(e) =>
          underlying match {
            case ms: mutable.Set[a] =>
              ms.remove(e.asInstanceOf[a])
              prev = None
            case _ =>
              throw new UnsupportedOperationException("remove")
          }
        case _ => throw new IllegalStateException("next must be called at least once before remove")
      }
    }
  }

  case class MutableSetWrapper[A](underlying : mutable.Set[A]) extends SetWrapper[A](underlying) {
    override def add(elem: A) = { val sz = underlying.size ; underlying += elem ; sz < underlying.size }
    override def remove(elem : AnyRef) = try {
      underlying.remove(elem.asInstanceOf[A])
    } catch {
      case ex: ClassCastException => false
    }
    override def clear() = underlying.clear()
  }

  case class JSetWrapper[A](underlying : ju.Set[A]) extends mutable.Set[A] with mutable.SetLike[A, JSetWrapper[A]] {
    override def size = underlying.size

    def iterator = underlying.iterator

    def contains(elem: A): Boolean = underlying.contains(elem)

    def +=(elem: A): this.type = { underlying.add(elem); this }
    def -=(elem: A): this.type = { underlying.remove(elem); this }

    override def add(elem: A): Boolean = underlying.add(elem)
    override def remove(elem: A): Boolean = underlying.remove(elem)
    override def clear() = underlying.clear()

    override def empty = JSetWrapper(new ju.HashSet[A])
  }

  class MapWrapper[A, B](underlying: Map[A, B]) extends ju.AbstractMap[A, B] { self =>
    override def size = underlying.size

    override def get(key: AnyRef): B = try {
      underlying get key.asInstanceOf[A] match {
        case None => null.asInstanceOf[B]
        case Some(v) => v
      }
    } catch {
      case ex: ClassCastException => null.asInstanceOf[B]
    }

    override def entrySet: ju.Set[ju.Map.Entry[A, B]] = new ju.AbstractSet[ju.Map.Entry[A, B]] {
      def size = self.size

      def iterator = new ju.Iterator[ju.Map.Entry[A, B]] {
        val ui = underlying.iterator
        var prev : Option[A] = None

        def hasNext = ui.hasNext

        def next = {
          val (k, v) = ui.next
          prev = Some(k)
          new ju.Map.Entry[A, B] {
            def getKey = k
            def getValue = v
            def setValue(v1 : B) = self.put(k, v1)
            override def equals(other : Any) = other match {
              case e : ju.Map.Entry[_, _] => k == e.getKey && v == e.getValue
              case _ => false
            }
          }
        }

        def remove = prev match {
          case Some(k) =>
            underlying match {
              case mm: mutable.Map[a, _] =>
                val v = mm.remove(k.asInstanceOf[a])
                prev = None
                v
              case _ =>
                throw new UnsupportedOperationException("remove")
            }
          case _ =>
            throw new IllegalStateException("next must be called at least once before remove")
        }
      }
    }
  }

  case class MutableMapWrapper[A, B](underlying: mutable.Map[A, B])
  extends MapWrapper[A, B](underlying) {
    override def put(k : A, v : B) = underlying.put(k, v) match {
      case Some(v1) => v1
      case None => null.asInstanceOf[B]
    }

    override def remove(k : AnyRef): B = try {
      underlying.remove(k.asInstanceOf[A]) match {
        case None => null.asInstanceOf[B]
        case Some(v) => v
      }
    } catch {
      case ex: ClassCastException => null.asInstanceOf[B]
    }

    override def clear() = underlying.clear()
  }

  trait JMapWrapperLike[A, B, +Repr <: mutable.MapLike[A, B, Repr] with mutable.Map[A, B]]
  extends mutable.Map[A, B] with mutable.MapLike[A, B, Repr] {
    def underlying: ju.Map[A, B]

    override def size = underlying.size

    def get(k : A) = {
      val v = underlying.get(k)
      if (v != null)
        Some(v)
      else if(underlying.containsKey(k))
        Some(null.asInstanceOf[B])
      else
        None
    }

    def +=(kv: (A, B)): this.type = { underlying.put(kv._1, kv._2); this }
    def -=(key: A): this.type = { underlying.remove(key); this }

    override def put(k : A, v : B): Option[B] = {
      val r = underlying.put(k, v)
      if (r != null) Some(r) else None
    }

    override def update(k : A, v : B) { underlying.put(k, v) }

    override def remove(k : A): Option[B] = {
      val r = underlying.remove(k)
      if (r != null) Some(r) else None
    }

    def iterator = new Iterator[(A, B)] {
      val ui = underlying.entrySet.iterator
      def hasNext = ui.hasNext
      def next = { val e = ui.next ; (e.getKey, e.getValue) }
    }

    override def clear() = underlying.clear()

    override def empty: Repr = null.asInstanceOf[Repr]
  }

  case class JMapWrapper[A, B](val underlying : ju.Map[A, B])
  extends JMapWrapperLike[A, B, JMapWrapper[A, B]] {
    override def empty = JMapWrapper(new ju.HashMap[A, B])
  }

  class ConcurrentMapWrapper[A, B](override val underlying: mutable.ConcurrentMap[A, B])
  extends MutableMapWrapper[A, B](underlying) with juc.ConcurrentMap[A, B] {

    def putIfAbsent(k: A, v: B) = underlying.putIfAbsent(k, v) match {
      case Some(v) => v
      case None => null.asInstanceOf[B]
    }

    def remove(k: AnyRef, v: AnyRef) = try {
      underlying.remove(k.asInstanceOf[A], v.asInstanceOf[B])
    } catch {
      case ex: ClassCastException =>
        false
    }

    def replace(k: A, v: B): B = underlying.replace(k, v) match {
      case Some(v) => v
      case None => null.asInstanceOf[B]
    }

    def replace(k: A, oldval: B, newval: B) = underlying.replace(k, oldval, newval)
  }

  case class JConcurrentMapWrapper[A, B](val underlying: juc.ConcurrentMap[A, B])
  extends JMapWrapperLike[A, B, JConcurrentMapWrapper[A, B]] with mutable.ConcurrentMap[A, B] {
    override def get(k: A) = {
      val v = underlying.get(k)
      if (v != null) Some(v)
      else None
    }

    override def empty = new JConcurrentMapWrapper(new juc.ConcurrentHashMap[A, B])

    def putIfAbsent(k: A, v: B): Option[B] = {
      val r = underlying.putIfAbsent(k, v)
      if (r != null) Some(r) else None
    }

    def remove(k: A, v: B): Boolean = underlying.remove(k, v)

    def replace(k: A, v: B): Option[B] = {
      val prev = underlying.replace(k, v)
      if (prev != null) Some(prev) else None
    }

    def replace(k: A, oldvalue: B, newvalue: B): Boolean = underlying.replace(k, oldvalue, newvalue)
  }

  case class DictionaryWrapper[A, B](underlying: mutable.Map[A, B])
  extends ju.Dictionary[A, B] {
    def size: Int = underlying.size
    def isEmpty: Boolean = underlying.isEmpty
    def keys: ju.Enumeration[A] = asJavaEnumeration(underlying.keysIterator)
    def elements: ju.Enumeration[B] = asJavaEnumeration(underlying.valuesIterator)
    def get(key: AnyRef) = try {
      underlying.get(key.asInstanceOf[A]) match {
        case None => null.asInstanceOf[B]
        case Some(v) => v
      }
    } catch {
      case ex: ClassCastException => null.asInstanceOf[B]
    }
    def put(key: A, value: B): B = underlying.put(key, value) match {
      case Some(v) => v
      case None => null.asInstanceOf[B]
    }
    override def remove(key: AnyRef) = try {
      underlying.remove(key.asInstanceOf[A]) match {
        case None => null.asInstanceOf[B]
        case Some(v) => v
      }
    } catch {
      case ex: ClassCastException => null.asInstanceOf[B]
    }
  }

  case class JDictionaryWrapper[A, B](underlying: ju.Dictionary[A, B])
  extends mutable.Map[A, B] {

    override def size: Int = underlying.size

    def get(k : A) = {
      val v = underlying.get(k)
      if (v != null) Some(v) else None
    }

    def +=(kv: (A, B)): this.type = { underlying.put(kv._1, kv._2); this }
    def -=(key: A): this.type = { underlying.remove(key); this }

    override def put(k : A, v : B): Option[B] = {
      val r = underlying.put(k, v)
      if (r != null) Some(r) else None
    }

    override def update(k : A, v : B) { underlying.put(k, v) }

    override def remove(k : A): Option[B] = {
      val r = underlying.remove(k)
      if (r != null) Some(r) else None
    }

    def iterator = enumerationAsScalaIterator(underlying.keys) map (k => (k, underlying get k))

    override def clear() = underlying.clear()
  }

  case class JPropertiesWrapper(underlying: ju.Properties)
  extends mutable.Map[String, String] with mutable.MapLike[String, String, JPropertiesWrapper] {
    override def size = underlying.size

    def get(k : String) = {
      val v = underlying.get(k)
      if (v != null)
        Some(v.asInstanceOf[String])
      else
        None
    }

    def +=(kv: (String, String)): this.type = { underlying.put(kv._1, kv._2); this }
    def -=(key: String): this.type = { underlying.remove(key); this }

    override def put(k : String, v : String): Option[String] = {
      val r = underlying.put(k, v)
      if (r != null) Some(r.asInstanceOf[String]) else None
    }

    override def update(k : String, v : String) { underlying.put(k, v) }

    override def remove(k : String): Option[String] = {
      val r = underlying.remove(k)
      if (r != null) Some(r.asInstanceOf[String]) else None
    }

    def iterator = new Iterator[(String, String)] {
      val ui = underlying.entrySet.iterator
      def hasNext = ui.hasNext
      def next = { val e = ui.next ; (e.getKey.asInstanceOf[String], e.getValue.asInstanceOf[String]) }
    }

    override def clear() = underlying.clear()

    override def empty = JPropertiesWrapper(new ju.Properties)

    def getProperty(key: String) = underlying.getProperty(key)

    def getProperty(key: String, defaultValue: String) = underlying.getProperty(key, defaultValue)

    def setProperty(key: String, value: String) = underlying.setProperty(key, value)
  }
}

