/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.collection

import scala.language.implicitConversions

/** Decorator representing lazily zipped pairs.
  *
  * @define coll pair
  * @define willNotTerminateInf
  *
  *              Note: will not terminate for infinite-sized collections.
  */
final class LazyZip2[+El1, +El2, C1] private[collection](src: C1, coll1: Iterable[El1], coll2: Iterable[El2]) {

  /** Zips `that` iterable collection with an existing `LazyZip2`. The elements in each collection are
    * not consumed until a strict operation is invoked on the returned `LazyZip3` decorator.
    *
    * @param that the iterable providing the third element of each eventual triple
    * @tparam B the type of the third element in each eventual triple
    * @return a decorator `LazyZip3` that allows strict operations to be performed on the lazily evaluated tuples or
    *         chained calls to `lazyZip`. Implicit conversion to `Iterable[(El1, El2, B)]` is also supported.
    */
  def lazyZip[B](that: Iterable[B]): LazyZip3[El1, El2, B, C1] = new LazyZip3(src, coll1, coll2, that)

  def map[B, C](f: (El1, El2) => B)(implicit bf: BuildFrom[C1, B, C]): C = {
    bf.fromSpecific(src)(new AbstractView[B] {
      def iterator: AbstractIterator[B] = new AbstractIterator[B] {
        private[this] val elems1 = coll1.iterator
        private[this] val elems2 = coll2.iterator
        def hasNext = elems1.hasNext && elems2.hasNext
        def next() = f(elems1.next(), elems2.next())
      }
      override def knownSize: Int = zipKnownSize
      override def isEmpty: Boolean = coll1.isEmpty || coll2.isEmpty
    })
  }

  def flatMap[B, C](f: (El1, El2) => Iterable[B])(implicit bf: BuildFrom[C1, B, C]): C = {
    bf.fromSpecific(src)(new AbstractView[B] {
      def iterator: AbstractIterator[B] = new AbstractIterator[B] {
        private[this] val elems1 = coll1.iterator
        private[this] val elems2 = coll2.iterator
        private[this] var _current: Iterator[B] = Iterator.empty
        private def current = {
          while (!_current.hasNext && elems1.hasNext && elems2.hasNext)
            _current = f(elems1.next(), elems2.next()).iterator
          _current
        }
        def hasNext = current.hasNext
        def next() = current.next()
      }
      override def knownSize: Int = if (coll1.knownSize == 0 || coll2.knownSize == 0) 0 else super.knownSize
      override def isEmpty: Boolean = coll1.isEmpty || coll2.isEmpty
    })
  }

  def filter[C](p: (El1, El2) => Boolean)(implicit bf: BuildFrom[C1, (El1, El2), C]): C = {
    bf.fromSpecific(src)(new AbstractView[(El1, El2)] {
      def iterator: AbstractIterator[(El1, El2)] = new AbstractIterator[(El1, El2)] {
        private[this] val elems1 = coll1.iterator
        private[this] val elems2 = coll2.iterator
        private[this] var _current: (El1, El2) = _
        private def current = {
          while ((_current eq null) && elems1.hasNext && elems2.hasNext) {
            val e1 = elems1.next()
            val e2 = elems2.next()
            if (p(e1, e2)) _current = (e1, e2)
          }
          _current
        }
        def hasNext = current ne null
        def next() = {
          val c = current
          if (c ne null) {
            _current = null
            c
          } else Iterator.empty.next()
        }
      }
      override def knownSize: Int = if (coll1.knownSize == 0 || coll2.knownSize == 0) 0 else super.knownSize
      override def isEmpty: Boolean = iterator.hasNext
    })
  }

  def exists(p: (El1, El2) => Boolean): Boolean = {
    val elems1 = coll1.iterator
    val elems2 = coll2.iterator
    var res = false

    while (!res && elems1.hasNext && elems2.hasNext) res = p(elems1.next(), elems2.next())

    res
  }

  def forall(p: (El1, El2) => Boolean): Boolean = !exists((el1, el2) => !p(el1, el2))

  def foreach[U](f: (El1, El2) => U): Unit = {
    val elems1 = coll1.iterator
    val elems2 = coll2.iterator

    while (elems1.hasNext && elems2.hasNext) f(elems1.next(), elems2.next())
  }

  private def toIterable: View[(El1, El2)] = new AbstractView[(El1, El2)] {
    def iterator: AbstractIterator[(El1, El2)] = new AbstractIterator[(El1, El2)] {
      private[this] val elems1 = coll1.iterator
      private[this] val elems2 = coll2.iterator
      def hasNext = elems1.hasNext && elems2.hasNext
      def next() = (elems1.next(), elems2.next())
    }
    override def knownSize: Int = zipKnownSize
    override def isEmpty: Boolean = coll1.isEmpty || coll2.isEmpty
  }

  private def zipKnownSize: Int = {
    val s1 = coll1.knownSize
    if (s1 == 0) 0 else {
      val s2 = coll2.knownSize
      if (s2 == 0) 0 else s1 min s2
    }
  }

  override def toString = s"$coll1.lazyZip($coll2)"
}

object LazyZip2 {
  implicit def lazyZip2ToIterable[El1, El2](zipped2: LazyZip2[El1, El2, _]): View[(El1, El2)] = zipped2.toIterable
}


/** Decorator representing lazily zipped triples.
  *
  * @define coll triple
  * @define willNotTerminateInf
  *
  *              Note: will not terminate for infinite-sized collections.
  */
final class LazyZip3[+El1, +El2, +El3, C1] private[collection](src: C1,
                                                               coll1: Iterable[El1],
                                                               coll2: Iterable[El2],
                                                               coll3: Iterable[El3]) {

  /** Zips `that` iterable collection with an existing `LazyZip3`. The elements in each collection are
    * not consumed until a strict operation is invoked on the returned `LazyZip4` decorator.
    *
    * @param that the iterable providing the fourth element of each eventual 4-tuple
    * @tparam B the type of the fourth element in each eventual 4-tuple
    * @return a decorator `LazyZip4` that allows strict operations to be performed on the lazily evaluated tuples.
    *         Implicit conversion to `Iterable[(El1, El2, El3, B)]` is also supported.
    */
  def lazyZip[B](that: Iterable[B]): LazyZip4[El1, El2, El3, B, C1] = new LazyZip4(src, coll1, coll2, coll3, that)

  def map[B, C](f: (El1, El2, El3) => B)(implicit bf: BuildFrom[C1, B, C]): C = {
    bf.fromSpecific(src)(new AbstractView[B] {
      def iterator: AbstractIterator[B] = new AbstractIterator[B] {
        private[this] val elems1 = coll1.iterator
        private[this] val elems2 = coll2.iterator
        private[this] val elems3 = coll3.iterator
        def hasNext = elems1.hasNext && elems2.hasNext && elems3.hasNext
        def next() = f(elems1.next(), elems2.next(), elems3.next())
      }
      override def knownSize: Int = zipKnownSize
      override def isEmpty: Boolean = coll1.isEmpty || coll2.isEmpty || coll3.isEmpty
    })
  }

  def flatMap[B, C](f: (El1, El2, El3) => Iterable[B])(implicit bf: BuildFrom[C1, B, C]): C = {
    bf.fromSpecific(src)(new AbstractView[B] {
      def iterator: AbstractIterator[B] = new AbstractIterator[B] {
        private[this] val elems1 = coll1.iterator
        private[this] val elems2 = coll2.iterator
        private[this] val elems3 = coll3.iterator
        private[this] var _current: Iterator[B] = Iterator.empty
        private def current = {
          while (!_current.hasNext && elems1.hasNext && elems2.hasNext && elems3.hasNext)
            _current = f(elems1.next(), elems2.next(), elems3.next()).iterator
          _current
        }
        def hasNext = current.hasNext
        def next() = current.next()
      }
      override def knownSize: Int = if (coll1.knownSize == 0 || coll2.knownSize == 0 || coll3.knownSize == 0) 0 else super.knownSize
      override def isEmpty: Boolean = iterator.isEmpty
    })
  }

  def filter[C](p: (El1, El2, El3) => Boolean)(implicit bf: BuildFrom[C1, (El1, El2, El3), C]): C = {
    bf.fromSpecific(src)(new AbstractView[(El1, El2, El3)] {
      def iterator: AbstractIterator[(El1, El2, El3)] = new AbstractIterator[(El1, El2, El3)] {
        private[this] val elems1 = coll1.iterator
        private[this] val elems2 = coll2.iterator
        private[this] val elems3 = coll3.iterator
        private[this] var _current: (El1, El2, El3) = _
        private def current = {
          while ((_current eq null) && elems1.hasNext && elems2.hasNext && elems3.hasNext) {
            val e1 = elems1.next()
            val e2 = elems2.next()
            val e3 = elems3.next()
            if (p(e1, e2, e3)) _current = (e1, e2, e3)
          }
          _current
        }
        def hasNext = current ne null
        def next() = {
          val c = current
          if (c ne null) {
            _current = null
            c
          } else Iterator.empty.next()
        }
      }
      override def knownSize: Int = if (coll1.knownSize == 0 || coll2.knownSize == 0 || coll3.knownSize == 0) 0 else super.knownSize
      override def isEmpty: Boolean = iterator.isEmpty
    })
  }

  def exists(p: (El1, El2, El3) => Boolean): Boolean = {
    val elems1 = coll1.iterator
    val elems2 = coll2.iterator
    val elems3 = coll3.iterator
    var res = false

    while (!res && elems1.hasNext && elems2.hasNext && elems3.hasNext)
      res = p(elems1.next(), elems2.next(), elems3.next())

    res
  }

  def forall(p: (El1, El2, El3) => Boolean): Boolean = !exists((el1, el2, el3) => !p(el1, el2, el3))

  def foreach[U](f: (El1, El2, El3) => U): Unit = {
    val elems1 = coll1.iterator
    val elems2 = coll2.iterator
    val elems3 = coll3.iterator

    while (elems1.hasNext && elems2.hasNext && elems3.hasNext)
      f(elems1.next(), elems2.next(), elems3.next())
  }

  private def toIterable: View[(El1, El2, El3)] = new AbstractView[(El1, El2, El3)] {
    def iterator: AbstractIterator[(El1, El2, El3)] = new AbstractIterator[(El1, El2, El3)] {
      private[this] val elems1 = coll1.iterator
      private[this] val elems2 = coll2.iterator
      private[this] val elems3 = coll3.iterator
      def hasNext = elems1.hasNext && elems2.hasNext && elems3.hasNext
      def next() = (elems1.next(), elems2.next(), elems3.next())
    }
    override def knownSize: Int = zipKnownSize
    override def isEmpty: Boolean = coll1.isEmpty || coll2.isEmpty || coll3.isEmpty
  }

  private def zipKnownSize: Int = {
    val s1 = coll1.knownSize
    if (s1 == 0) 0 else {
      val s2 = coll2.knownSize
      if (s2 == 0) 0 else {
        val s3 = coll3.knownSize
        if (s3 == 0) 0 else s1 min s2 min s3
      }
    }
  }

  override def toString = s"$coll1.lazyZip($coll2).lazyZip($coll3)"
}

object LazyZip3 {
  implicit def lazyZip3ToIterable[El1, El2, El3](zipped3: LazyZip3[El1, El2, El3, _]): View[(El1, El2, El3)] = zipped3.toIterable
}



/** Decorator representing lazily zipped 4-tuples.
  *
  * @define coll tuple
  * @define willNotTerminateInf
  *
  *              Note: will not terminate for infinite-sized collections.
  */
final class LazyZip4[+El1, +El2, +El3, +El4, C1] private[collection](src: C1,
                                                                     coll1: Iterable[El1],
                                                                     coll2: Iterable[El2],
                                                                     coll3: Iterable[El3],
                                                                     coll4: Iterable[El4]) {

  def map[B, C](f: (El1, El2, El3, El4) => B)(implicit bf: BuildFrom[C1, B, C]): C = {
    bf.fromSpecific(src)(new AbstractView[B] {
      def iterator: AbstractIterator[B] = new AbstractIterator[B] {
        private[this] val elems1 = coll1.iterator
        private[this] val elems2 = coll2.iterator
        private[this] val elems3 = coll3.iterator
        private[this] val elems4 = coll4.iterator
        def hasNext = elems1.hasNext && elems2.hasNext && elems3.hasNext && elems4.hasNext
        def next() = f(elems1.next(), elems2.next(), elems3.next(), elems4.next())
      }
      override def knownSize: Int = zipKnownSize
      override def isEmpty: Boolean = coll1.isEmpty || coll2.isEmpty || coll3.isEmpty || coll4.isEmpty
    })
  }

  def flatMap[B, C](f: (El1, El2, El3, El4) => Iterable[B])(implicit bf: BuildFrom[C1, B, C]): C = {
    bf.fromSpecific(src)(new AbstractView[B] {
      def iterator: AbstractIterator[B] = new AbstractIterator[B] {
        private[this] val elems1 = coll1.iterator
        private[this] val elems2 = coll2.iterator
        private[this] val elems3 = coll3.iterator
        private[this] val elems4 = coll4.iterator
        private[this] var _current: Iterator[B] = Iterator.empty
        private def current = {
          while (!_current.hasNext && elems1.hasNext && elems2.hasNext && elems3.hasNext && elems4.hasNext)
            _current = f(elems1.next(), elems2.next(), elems3.next(), elems4.next()).iterator
          _current
        }
        def hasNext = current.hasNext
        def next() = current.next()
      }
      override def knownSize: Int = if (coll1.knownSize == 0 || coll2.knownSize == 0 || coll3.knownSize == 0 || coll4.knownSize == 0) 0 else super.knownSize
      override def isEmpty: Boolean = iterator.isEmpty
    })
  }

  def filter[C](p: (El1, El2, El3, El4) => Boolean)(implicit bf: BuildFrom[C1, (El1, El2, El3, El4), C]): C = {
    bf.fromSpecific(src)(new AbstractView[(El1, El2, El3, El4)] {
      def iterator: AbstractIterator[(El1, El2, El3, El4)] = new AbstractIterator[(El1, El2, El3, El4)] {
        private[this] val elems1 = coll1.iterator
        private[this] val elems2 = coll2.iterator
        private[this] val elems3 = coll3.iterator
        private[this] val elems4 = coll4.iterator
        private[this] var _current: (El1, El2, El3, El4) = _
        private def current = {
          while ((_current eq null) && elems1.hasNext && elems2.hasNext && elems3.hasNext && elems4.hasNext) {
            val e1 = elems1.next()
            val e2 = elems2.next()
            val e3 = elems3.next()
            val e4 = elems4.next()
            if (p(e1, e2, e3, e4)) _current = (e1, e2, e3, e4)
          }
          _current
        }
        def hasNext = current ne null
        def next() = {
          val c = current
          if (c ne null) {
            _current = null
            c
          } else Iterator.empty.next()
        }
      }
      override def knownSize: Int = if (coll1.knownSize == 0 || coll2.knownSize == 0 || coll3.knownSize == 0 || coll4.knownSize == 0) 0 else super.knownSize
      override def isEmpty: Boolean = iterator.isEmpty
    })
  }

  def exists(p: (El1, El2, El3, El4) => Boolean): Boolean = {
    val elems1 = coll1.iterator
    val elems2 = coll2.iterator
    val elems3 = coll3.iterator
    val elems4 = coll4.iterator
    var res = false

    while (!res && elems1.hasNext && elems2.hasNext && elems3.hasNext && elems4.hasNext)
      res = p(elems1.next(), elems2.next(), elems3.next(), elems4.next())

    res
  }

  def forall(p: (El1, El2, El3, El4) => Boolean): Boolean = !exists((el1, el2, el3, el4) => !p(el1, el2, el3, el4))

  def foreach[U](f: (El1, El2, El3, El4) => U): Unit = {
    val elems1 = coll1.iterator
    val elems2 = coll2.iterator
    val elems3 = coll3.iterator
    val elems4 = coll4.iterator

    while (elems1.hasNext && elems2.hasNext && elems3.hasNext && elems4.hasNext)
      f(elems1.next(), elems2.next(), elems3.next(), elems4.next())
  }

  private def toIterable: View[(El1, El2, El3, El4)] = new AbstractView[(El1, El2, El3, El4)] {
    def iterator: AbstractIterator[(El1, El2, El3, El4)] = new AbstractIterator[(El1, El2, El3, El4)] {
      private[this] val elems1 = coll1.iterator
      private[this] val elems2 = coll2.iterator
      private[this] val elems3 = coll3.iterator
      private[this] val elems4 = coll4.iterator
      def hasNext = elems1.hasNext && elems2.hasNext && elems3.hasNext && elems4.hasNext
      def next() = (elems1.next(), elems2.next(), elems3.next(), elems4.next())
    }
    override def knownSize: Int = zipKnownSize
    override def isEmpty: Boolean = coll1.isEmpty || coll2.isEmpty || coll3.isEmpty || coll4.isEmpty
  }

  private def zipKnownSize: Int = {
    val s1 = coll1.knownSize
    if (s1 == 0) 0 else {
      val s2 = coll2.knownSize
      if (s2 == 0) 0 else {
        val s3 = coll3.knownSize
        if (s3 == 0) 0 else {
          val s4 = coll4.knownSize
          if (s4 == 0) 0 else s1 min s2 min s3 min s4
        }
      }
    }
  }

  override def toString = s"$coll1.lazyZip($coll2).lazyZip($coll3).lazyZip($coll4)"
}

object LazyZip4 {
  implicit def lazyZip4ToIterable[El1, El2, El3, El4](zipped4: LazyZip4[El1, El2, El3, El4, _]): View[(El1, El2, El3, El4)] =
    zipped4.toIterable
}
