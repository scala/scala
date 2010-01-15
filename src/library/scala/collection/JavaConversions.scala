/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


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
 *    <li><code>scala.collection.mutable.Map</code> <=> <code>java.util.Map</code></li>
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
 *    Note that no conversion is provided from <code>scala.collection.immutable.List</code>
 *    to <code>java.util.List</code>. Instead it is convertible to an immutable
 *    <code>java.util.Collection</code> which provides size and interation
 *    capabilities, but not access by index as would be provided by
 *    <code>java.util.List</code>.<br/>
 *    This is intentional: in combination the implementation of
 *    <code>scala.collection.immutable.List</code> and the typical usage
 *    patterns of <code>java.util.List</code> would perform extremely poorly.
 *  </p>
 *
 *  @author Miles Sabin
 *  @since  2.8
 */
object JavaConversions {
  import java.{ lang => jl, util => ju }
  import java.util.{ concurrent => juc }
  import scala.collection.{ generic, immutable, mutable, Traversable }
  import scala.reflect.ClassManifest

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
  implicit def asSet[A](s : mutable.Set[A])(implicit m : ClassManifest[A]) : ju.Set[A] = s match {
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
  implicit def asMap[A, B](m : mutable.Map[A, B])(implicit ma : ClassManifest[A]) : ju.Map[A, B] = m match {
    //case JConcurrentMapWrapper(wrapped) => wrapped
    case JMapWrapper(wrapped) => wrapped
    case _ => new MutableMapWrapper(m)(ma)
  }

  implicit def asConcurrentMap[A, B](m: mutable.ConcurrentMap[A, B])
    (implicit ma: ClassManifest[A], mb: ClassManifest[B]): juc.ConcurrentMap[A, B] = m match {
    case JConcurrentMapWrapper(wrapped) => wrapped
    case _ => new ConcurrentMapWrapper(m)(ma, mb)
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
    //case ConcurrentMapWrapper(wrapped) => wrapped
    case MutableMapWrapper(wrapped) => wrapped
    case _ => new JMapWrapper(m)
  }

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
  implicit def asConcurrentMap[A, B](m: juc.ConcurrentMap[A, B]) = m match {
    case ConcurrentMapWrapper(wrapped) => wrapped
    case _ => new JConcurrentMapWrapper(m)
  }

  implicit def asMap(p: ju.Properties): mutable.Map[String, String] = p match {
    case _ => new JPropertiesWrapper(p)
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

  case class MutableSetWrapper[A](underlying : mutable.Set[A])(m : ClassManifest[A]) extends ju.AbstractSet[A] {
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
      val ui = underlying.iterator
      var prev : Option[A] = None

      def hasNext = ui.hasNext
      def next = { val e = ui.next ; prev = Some(e) ; e }
      def remove = prev match {
        case Some(e) => self.remove(e.asInstanceOf[AnyRef]) ; prev = None
        case _ => throw new IllegalStateException("next must be called at least once before remove")
      }
    }

  }

  case class JSetWrapper[A](underlying : ju.Set[A]) extends mutable.Set[A] with mutable.SetLike[A, JSetWrapper[A]] {
    override def size = underlying.size

    def iterator = underlying.iterator

    def contains(elem: A): Boolean = underlying.contains(elem)

    def +=(elem: A): this.type = { underlying.add(elem); this }
    def -=(elem: A): this.type = { underlying.remove(elem); this }

    override def add(elem: A): Boolean = underlying.add(elem)
    override def remove(elem: A): Boolean = underlying.remove(elem)

    override def clear = underlying.clear

    override def empty = JSetWrapper(new ju.HashSet[A])
  }

  abstract class MutableMapWrapperLike[A, B](underlying: mutable.Map[A, B])(m: ClassManifest[A])
  extends ju.AbstractMap[A, B] {
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
          case Some(k) => val v = self.remove(k.asInstanceOf[AnyRef]) ; prev = None ; v
          case _ => throw new IllegalStateException("next must be called at least once before remove")
        }
      }
    }
  }

  case class MutableMapWrapper[A, B](underlying : mutable.Map[A, B])(m : ClassManifest[A])
  extends MutableMapWrapperLike[A, B](underlying)(m)

  abstract class JMapWrapperLike[A, B, +Repr <: mutable.MapLike[A, B, Repr] with mutable.Map[A, B]]
    (underlying: ju.Map[A, B])
  extends mutable.Map[A, B] with mutable.MapLike[A, B, Repr] {
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

    override def clear = underlying.clear

    override def empty: Repr = null.asInstanceOf[Repr]
  }

  case class JMapWrapper[A, B](underlying : ju.Map[A, B])
  extends JMapWrapperLike[A, B, JMapWrapper[A, B]](underlying) {
    override def empty = JMapWrapper(new ju.HashMap[A, B])
  }

  case class ConcurrentMapWrapper[A, B](underlying: mutable.ConcurrentMap[A, B])
    (m: ClassManifest[A], mv: ClassManifest[B])
  extends MutableMapWrapperLike[A, B](underlying)(m) with juc.ConcurrentMap[A, B] {
    self =>

    override def remove(k : AnyRef) = {
      if (!m.erasure.isInstance(k))
        null.asInstanceOf[B]
      else {
        val k1 = k.asInstanceOf[A]
        underlying.remove(k1) match {
          case Some(v) => v
          case None => null.asInstanceOf[B]
        }
      }
    }

    def putIfAbsent(k: A, v: B) = underlying.putIfAbsent(k, v) match {
      case Some(v) => v
      case None => null.asInstanceOf[B]
    }

    def remove(k: AnyRef, v: AnyRef) = {
      if (!m.erasure.isInstance(k) || !mv.erasure.isInstance(v))
        false
      else {
        val k1 = k.asInstanceOf[A]
        val v1 = v.asInstanceOf[B]
        underlying.remove(k1, v1)
      }
    }

    def replace(k: A, v: B): B = underlying.replace(k, v) match {
      case Some(v) => v
      case None => null.asInstanceOf[B]
    }

    def replace(k: A, oldval: B, newval: B) = underlying.replace(k, oldval, newval)

  }

  case class JConcurrentMapWrapper[A, B](underlying: juc.ConcurrentMap[A, B])
  extends JMapWrapperLike[A, B, JConcurrentMapWrapper[A, B]](underlying) with mutable.ConcurrentMap[A, B] {
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

    override def clear = underlying.clear

    override def empty = JPropertiesWrapper(new ju.Properties)

    def getProperty(key: String) = underlying.getProperty(key)

    def getProperty(key: String, defaultValue: String) = underlying.getProperty(key, defaultValue)

    def setProperty(key: String, value: String) = underlying.setProperty(key, value)
  }

}




