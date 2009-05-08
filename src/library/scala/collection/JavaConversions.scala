/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection

/**
 * A collection of implicit conversions supporting interoperability between Scala and Java
 * collections.
 * <p>
 * The following conversions are supported,
 * <p>
 * <ul>
 *   <li><code>scala.collection.Iterable</code> <=> <code>java.lang.Iterable</code></li>
 *   <li><code>scala.collection.Iterable</code> <=> <code>java.util.Collection</code></li>
 *   <li><code>scala.collection.Iterator</code> <=> <code>java.util.{ Iterator, Enumeration }</code></li>
 *   <li<code>>scala.collection.mutable.Buffer</code> <=> <code>java.util.List</code></li>
 *   <li<code>>scala.collection.mutable.Set</code> <=> <code>java.util.Set</code></li>
 *   <li<code>>scala.collection.mutable.Map</code> <=> <code>java.util.Map</code></li>
 * <ul>
 * <p>
 * In all cases, converting from a source type to a target type and back again will return
 * the original source object, eg.
 * <p>
 * <pre>
 * import scala.collections.jcl.Conversions._
 *
 * val sl = new scala.collection.mutable.ListBuffer[Int]
 * val jl : java.util.List[Int] = sl
 * val sl2 : scala.collection.mutable.Buffer[Int] = jl
 * assert(sl eq sl2)g
 * </pre>
 *
 * <p>
 * Note that no conversion is provided from scala.collection.immutable.List to java.util.List.
 * Instead it is convertible to an immutable java.util.Collection which provides size and
 * interation capabilities, but not access by index as would be provided by java.util.List.
 * This is intentional: in combination the implementation of scala.collection.immutable.List
 * and the typical usage patterns of java.util.List would perform extremely poorly.
 *
 * @author Miles Sabin
 */
object JavaConversions {
  import java.{ lang => jl, util => ju }
  import scala.collection.{ generic, immutable, mutable, Traversible }
  import scala.reflect.Manifest

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
  implicit def asIterator[A](i : Iterator[A]) = i match {
    case JIteratorWrapper(wrapped) => wrapped
    case _ => IteratorWrapper(i)
  }

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
  implicit def asEnumeration[A](i : Iterator[A]) = i match {
    case JEnumerationWrapper(wrapped) => wrapped
    case _ => IteratorWrapper(i)
  }

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
  implicit def asIterable[A](i : Iterable[A]) = i match {
    case JIterableWrapper(wrapped) => wrapped
    case _ => IterableWrapper(i)
  }

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
  implicit def asCollection[A](i : Iterable[A]) = i match {
    case JCollectionWrapper(wrapped) => wrapped
    case _ => new IterableWrapper(i)
  }

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
  implicit def asList[A](b : mutable.Buffer[A]) : ju.List[A] = b match {
    case JListWrapper(wrapped) => wrapped
    case _ => new MutableBufferWrapper(b)
  }

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
  implicit def asSet[A](s : mutable.Set[A])(implicit m : Manifest[A]) : ju.Set[A] = s match {
    case JSetWrapper(wrapped) => wrapped
    case _ => new MutableSetWrapper(s)(m)
  }

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
  implicit def asMap[A, B](m : mutable.Map[A, B])(implicit ma : Manifest[A]) : ju.Map[A, B] = m match {
    case JMapWrapper(wrapped) => wrapped
    case _ => new MutableMapWrapper(m)(ma)
  }

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
  implicit def asIterator[A](i : ju.Iterator[A]) = i match {
    case IteratorWrapper(wrapped) => wrapped
    case _ => JIteratorWrapper(i)
  }

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
  implicit def asIterator[A](i : ju.Enumeration[A]) = i match {
    case IteratorWrapper(wrapped) => wrapped
    case _ => JEnumerationWrapper(i)
  }

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
  implicit def asIterable[A](i : jl.Iterable[A]) = i match {
    case IterableWrapper(wrapped) => wrapped
    case _ => JIterableWrapper(i)
  }

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
  implicit def asIterable[A](i : ju.Collection[A]) = i match {
    case IterableWrapper(wrapped) => wrapped
    case _ => JCollectionWrapper(i)
  }

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
  implicit def asBuffer[A](l : ju.List[A]) = l match {
    case MutableBufferWrapper(wrapped) => wrapped
    case _ =>new JListWrapper(l)
  }

  /**
   * Implicitly converts a Java <code>Set</code> to a Scala mutable <code>Set</code>.
   * The returned Scala <code>Set</code> is backed by the provided Java
   * <code>Set</code> and any side-effects of using it via the Scala interface will
   * be visible via the Java interface and vice versa.
   * <p>
   * If the Java <code>Set</code> was previously obtained from an implicit or
   * explicit call of <code>asSet(scala.collection.mutable.Set)</code> then the original
   * Scala <code>Set</code> will be returned.
   *
   * @param s The <code>Set</code> to be converted.
   * @return A Scala mutable <code>Set</code> view of the argument.
   */
  implicit def asSet[A](s : ju.Set[A]) = s match {
    case MutableSetWrapper(wrapped) => wrapped
    case _ =>new JSetWrapper(s)
  }

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
  implicit def asMap[A, B](m : ju.Map[A, B]) = m match {
    case MutableMapWrapper(wrapped) => wrapped
    case _ =>new JMapWrapper(m)
  }

  // Private implementations ...

  case class IteratorWrapper[A](underlying : Iterator[A]) extends ju.Iterator[A] with ju.Enumeration[A] {
    def hasNext = underlying.hasNext
    def next = underlying.next
    def hasMoreElements = underlying.hasNext
    def nextElement = underlying.next
    def remove = throw new UnsupportedOperationException
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
    def iterator = underlying.elements
    def size = underlying.size
    override def isEmpty = underlying.isEmpty
  }

  case class JIterableWrapper[A](underlying : jl.Iterable[A]) extends Iterable[A] {
    def elements = underlying.iterator
    def newBuilder[B] = new mutable.ArrayBuffer[B]
  }

  case class JCollectionWrapper[A](underlying : ju.Collection[A]) extends Iterable[A] {
    def elements = underlying.iterator
    override def size = underlying.size
    override def isEmpty = underlying.isEmpty
    def newBuilder[B] = new mutable.ArrayBuffer[B]
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
    override def elements : Iterator[A] = underlying.iterator
    def apply(i : Int) = underlying.get(i)
    def update(i : Int, elem : A) = underlying.set(i, elem)
    def +:(elem : A) = { underlying.subList(0, 0).add(elem) ; this }
    def +=(elem : A) = underlying.add(elem)
    def insertAll(i : Int, elems : Traversible[A]) = { val ins = underlying.subList(0, i) ;  elems.foreach(ins.add(_)) }
    def remove(i : Int) = underlying.remove(i)
    def clear = underlying.clear
    def result = this
  }

  case class MutableSetWrapper[A](underlying : mutable.Set[A])(m : Manifest[A]) extends ju.AbstractSet[A] {
    self =>
    def size = underlying.size
    override def add(elem: A) = { val sz = underlying.size ; underlying += elem ; sz < underlying.size }
    override def remove(elem : AnyRef) = {
      m.erasure.isInstance(elem) && {
        val sz = underlying.size
        underlying -= elem.asInstanceOf[A]
        sz > underlying.size
      }
    }
    def iterator = new ju.Iterator[A] {
      val ui = underlying.elements
      var prev : Option[A] = None

      def hasNext = ui.hasNext
      def next = { val e = ui.next ; prev = Some(e) ; e }
      def remove = prev match {
        case Some(e) => self.remove(e.asInstanceOf[AnyRef]) ; prev = None
        case _ => throw new IllegalStateException("next must be called at least once before remove")
      }
    }
  }

  case class JSetWrapper[A](underlying : ju.Set[A]) extends mutable.Set[A] with generic.SetTemplate[A, JSetWrapper[A]] {
    override def size = underlying.size

    def elements = underlying.iterator

    def contains(elem: A): Boolean = underlying.contains(elem)

    def +=(elem: A) { underlying.add(elem) }

    def -=(elem: A) { underlying.remove(elem) }

    override def clear = underlying.clear

    override def empty = JSetWrapper(new ju.HashSet[A])
  }

  case class MutableMapWrapper[A, B](underlying : mutable.Map[A, B])(m : Manifest[A]) extends ju.AbstractMap[A, B] {
    self =>
    override def size = underlying.size

    override def put(k : A, v : B) = underlying.put(k, v) match {
      case Some(v1) => v1
      case None => null.asInstanceOf[B]
    }

    override def remove(k : AnyRef) = {
      if (!m.erasure.isInstance(k))
        null.asInstanceOf[B]
      else {
        val k1 = k.asInstanceOf[A]
        underlying.get(k1) match {
          case Some(v) => underlying -= k1 ; v
          case None => null.asInstanceOf[B]
        }
      }
    }

    override def entrySet : ju.Set[ju.Map.Entry[A, B]] = new ju.AbstractSet[ju.Map.Entry[A, B]] {
      def size = self.size

      def iterator = new ju.Iterator[ju.Map.Entry[A, B]] {
        val ui = underlying.elements
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
          case Some(k) => val v = self.remove(k.asInstanceOf[AnyRef]) ; prev = None ; v
          case _ => throw new IllegalStateException("next must be called at least once before remove")
        }
      }
    }
  }

  case class JMapWrapper[A, B](underlying : ju.Map[A, B]) extends mutable.Map[A, B] with generic.MutableMapTemplate[A, B, JMapWrapper[A, B]] {
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

    def update(k : A, v : B) { underlying.put(k, v) }

    def -=(k : A) = { underlying.remove(k) ; this }

    def elements = new Iterator[(A, B)] {
      val ui = underlying.entrySet.iterator
      def hasNext = ui.hasNext
      def next = { val e = ui.next ; (e.getKey, e.getValue) }
    }

    override def clear = underlying.clear

    override def empty = JMapWrapper(new ju.HashMap[A, B])
  }
}
