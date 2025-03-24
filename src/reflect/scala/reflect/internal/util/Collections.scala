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

package scala
package reflect.internal.util

import scala.reflect.ClassTag
import scala.collection.AbstractIterator
import scala.collection.{immutable, mutable}
import scala.annotation.tailrec
import mutable.ListBuffer
import scala.runtime.Statics.releaseFence

/** Profiler driven changes.
 *  TODO - inlining doesn't work from here because of the bug that
 *  methods in traits aren't inlined.
 */
trait Collections {
  /** True if all three arguments have the same number of elements and
   *  the function is true for all the triples.
   */
  @tailrec final def corresponds3[A, B, C](xs1: List[A], xs2: List[B], xs3: List[C])
        (f: (A, B, C) => Boolean): Boolean = (
    if (xs1.isEmpty) xs2.isEmpty && xs3.isEmpty
    else !xs2.isEmpty && !xs3.isEmpty && f(xs1.head, xs2.head, xs3.head) && corresponds3(xs1.tail, xs2.tail, xs3.tail)(f)
  )

  /** All these mm methods are "deep map" style methods for
   *  mapping etc. on a list of lists while avoiding unnecessary
   *  intermediate structures like those created via flatten.
   */
  final def mexists[A](xss: List[List[A]])(p: A => Boolean) =
    xss exists (_ exists p)
  final def mforall[A](xss: List[List[A]])(p: A => Boolean) =
    xss forall (_ forall p)
  final def mmap[A, B](xss: List[List[A]])(f: A => B) =
    xss map (_ map f)
  final def mfind[A](xss: List[List[A]])(p: A => Boolean): Option[A] = {
    var res: Option[A] = null
    mforeach(xss)(x => if ((res eq null) && p(x)) res = Some(x))
    if (res eq null) None else res
  }

  /** These are all written in terms of List because we're trying to wring all
   *  the performance we can and List is used almost exclusively in the compiler,
   *  but people are branching out in their collections so here's an overload.
   */
  final def mforeach[A](xss: List[List[A]])(f: A => Unit) = xss foreach (_ foreach f)
  final def mforeach[A](xss: Iterable[Iterable[A]])(f: A => Unit) = xss foreach (_ foreach f)

  /** A version of List#map, specialized for List, and optimized to avoid allocation if `as` is empty */
  final def mapList[A, B](as: List[A])(f: A => B): List[B] = if (as eq Nil) Nil else {
    val head = new ::[B](f(as.head), Nil)
    var tail: ::[B] = head
    var rest = as.tail
    while (rest ne Nil) {
      val next = new ::(f(rest.head), Nil)
      tail.next = next
      tail = next
      rest = rest.tail
    }
    releaseFence()
    head
  }

  final def sameElementsEquals(thiss: List[AnyRef], that: List[AnyRef]): Boolean = {
    // Probably immutable, so check reference identity first (it's quick anyway)
    (thiss eq that) || {
      var these = thiss
      var those = that
      while (!these.isEmpty && !those.isEmpty && these.head.equals(those.head)) {
        these = these.tail
        those = those.tail
      }
      these.isEmpty && those.isEmpty
    }
  }

  final def collectFirst[A, B](as: List[A])(pf: PartialFunction[A, B]): Option[B] = {
    def loop(as: List[A]): Option[B] = as match {
      case a :: as =>
        if (pf.isDefinedAt(a)) Some(pf(a))
        else loop(as)
      case _ => None
    }
    loop(as)
  }

  final def map2[A, B, C](xs1: List[A], xs2: List[B])(f: (A, B) => C): List[C] = {
    val lb = new ListBuffer[C]
    var ys1 = xs1
    var ys2 = xs2
    while (!ys1.isEmpty && !ys2.isEmpty) {
      lb += f(ys1.head, ys2.head)
      ys1 = ys1.tail
      ys2 = ys2.tail
    }
    lb.toList
  }

  /** like map2, but returns list `xs` itself - instead of a copy - if function
   *  `f` maps all elements to themselves.
   */
  final def map2Conserve[A <: AnyRef, B](xs: List[A], ys: List[B])(f: (A, B) => A): List[A] = {
    // Note to developers: there exists a duplication between this function and `List#mapConserve`.
    // If any successful optimization attempts or other changes are made, please rehash them there too.
    @tailrec
    def loop(mapped: ListBuffer[A], unchanged: List[A], pending0: List[A], pending1: List[B]): List[A] = {
      if (pending0.isEmpty || pending1.isEmpty) {
        if (mapped eq null) unchanged
        else mapped.prependToList(unchanged)
      } else {
        val head00 = pending0.head
        val head01 = pending1.head
        val head1  = f(head00, head01)

        if ((head1 eq head00.asInstanceOf[AnyRef])) {
          loop(mapped, unchanged, pending0.tail, pending1.tail)
        } else {
          val b = if (mapped eq null) new ListBuffer[A] else mapped
          var xc = unchanged
          while ((xc ne pending0) && (xc ne pending1)) {
            b += xc.head
            xc = xc.tail
          }
          b += head1
          val tail0 = pending0.tail
          val tail1 = pending1.tail
          loop(b, tail0, tail0, tail1)
        }
      }
    }
    val result = loop(null, xs, xs, ys)
    releaseFence()
    result
  }

  final def map3[A, B, C, D](xs1: List[A], xs2: List[B], xs3: List[C])(f: (A, B, C) => D): List[D] = {
    if (xs1.isEmpty || xs2.isEmpty || xs3.isEmpty) Nil
    else f(xs1.head, xs2.head, xs3.head) :: map3(xs1.tail, xs2.tail, xs3.tail)(f)
  }
  final def flatMap2[A, B, C](xs1: List[A], xs2: List[B])(f: (A, B) => List[C]): List[C] = {
    var lb: ListBuffer[C] = null
    var ys1 = xs1
    var ys2 = xs2
    while (!ys1.isEmpty && !ys2.isEmpty) {
      val cs = f(ys1.head, ys2.head)
      if (cs ne Nil) {
        if (lb eq null) lb = new ListBuffer[C]
        lb ++= cs
      }
      ys1 = ys1.tail
      ys2 = ys2.tail
    }
    if (lb eq null) Nil else lb.result()
  }

  // compare to foldLeft[A, B](xs)
  final def foldLeft2[A1, A2, B](xs1: List[A1], xs2: List[A2])(z0: B)(f: (B, A1, A2) => B): B = {
    var ys1 = xs1
    var ys2 = xs2
    var res = z0
    while (!ys1.isEmpty && !ys2.isEmpty) {
      res = f(res, ys1.head, ys2.head)
      ys1 = ys1.tail
      ys2 = ys2.tail
    }
    res
  }

  final def flatCollect[A, B](elems: List[A])(pf: PartialFunction[A, Iterable[B]]): List[B] = {
    val lb = new ListBuffer[B]
    for (x <- elems ; if pf isDefinedAt x)
      lb ++= pf(x)

    lb.toList
  }

  final def distinctBy[A, B](xs: List[A])(f: A => B): List[A] = {
    val buf = new ListBuffer[A]
    val seen = mutable.Set[B]()
    xs foreach { x =>
      val y = f(x)
      if (!seen(y)) {
        buf += x
        seen += y
      }
    }
    buf.toList
  }

  @tailrec final def flattensToEmpty(xss: Seq[Seq[_]]): Boolean = {
    xss.isEmpty || xss.head.isEmpty && flattensToEmpty(xss.tail)
  }

  final def foreachWithIndex[A](xs: List[A])(f: (A, Int) => Unit): Unit = {
    var index = 0
    var ys = xs
    while (!ys.isEmpty) {
      f(ys.head, index)
      ys = ys.tail
      index += 1
    }
  }

  // @inline
  final def findOrElse[A](xs: IterableOnce[A])(p: A => Boolean)(orElse: => A): A = {
    xs.iterator find p getOrElse orElse
  }

  final def mapFrom[A, A1 >: A, B](xs: List[A])(f: A => B): Map[A1, B] = {
    Map[A1, B](xs map (x => (x, f(x))): _*)
  }
  final def linkedMapFrom[A, A1 >: A, B](xs: List[A])(f: A => B): mutable.LinkedHashMap[A1, B] = {
    mutable.LinkedHashMap[A1, B](xs map (x => (x, f(x))): _*)
  }

  final def mapWithIndex[A, B](xs: List[A])(f: (A, Int) => B): List[B] = {
    val lb = new ListBuffer[B]
    var index = 0
    var ys = xs
    while (!ys.isEmpty) {
      lb += f(ys.head, index)
      ys = ys.tail
      index += 1
    }
    lb.toList
  }
  final def collectMap2[A, B, C](xs1: List[A], xs2: List[B])(p: (A, B) => Boolean): Map[A, B] = {
    if (xs1.isEmpty || xs2.isEmpty)
      return Map()

    val buf = immutable.Map.newBuilder[A, B]
    var ys1 = xs1
    var ys2 = xs2
    while (!ys1.isEmpty && !ys2.isEmpty) {
      val x1 = ys1.head
      val x2 = ys2.head
      if (p(x1, x2))
        buf += ((x1, x2))

      ys1 = ys1.tail
      ys2 = ys2.tail
    }
    buf.result()
  }
  final def foreach2[A, B](xs1: List[A], xs2: List[B])(f: (A, B) => Unit): Unit = {
    var ys1 = xs1
    var ys2 = xs2
    while (!ys1.isEmpty && !ys2.isEmpty) {
      f(ys1.head, ys2.head)
      ys1 = ys1.tail
      ys2 = ys2.tail
    }
  }
  final def foreach3[A, B, C](xs1: List[A], xs2: List[B], xs3: List[C])(f: (A, B, C) =>  Unit): Unit = {
    var ys1 = xs1
    var ys2 = xs2
    var ys3 = xs3
    while (!ys1.isEmpty && !ys2.isEmpty && !ys3.isEmpty) {
      f(ys1.head, ys2.head, ys3.head)
      ys1 = ys1.tail
      ys2 = ys2.tail
      ys3 = ys3.tail
    }
  }
  final def exists2[A, B](xs1: List[A], xs2: List[B])(f: (A, B) => Boolean): Boolean = {
    var ys1 = xs1
    var ys2 = xs2
    while (!ys1.isEmpty && !ys2.isEmpty) {
      if (f(ys1.head, ys2.head))
        return true

      ys1 = ys1.tail
      ys2 = ys2.tail
    }
    false
  }
  final def exists3[A, B, C](xs1: List[A], xs2: List[B], xs3: List[C])(f: (A, B, C) => Boolean): Boolean = {
    var ys1 = xs1
    var ys2 = xs2
    var ys3 = xs3
    while (!ys1.isEmpty && !ys2.isEmpty && !ys3.isEmpty) {
      if (f(ys1.head, ys2.head, ys3.head))
        return true

      ys1 = ys1.tail
      ys2 = ys2.tail
      ys3 = ys3.tail
    }
    false
  }
  final def forall3[A, B, C](xs1: List[A], xs2: List[B], xs3: List[C])(f: (A, B, C) => Boolean): Boolean = {
    var ys1 = xs1
    var ys2 = xs2
    var ys3 = xs3
    while (!ys1.isEmpty && !ys2.isEmpty && !ys3.isEmpty) {
      if (!f(ys1.head, ys2.head, ys3.head))
        return false

      ys1 = ys1.tail
      ys2 = ys2.tail
      ys3 = ys3.tail
    }
    true
  }

  final def mapFilter2[A, B, C](itA: Iterator[A], itB: Iterator[B])(f: (A, B) => Option[C]): Iterator[C] =
    new AbstractIterator[C] {
      private[this] var head: Option[C] = None
      private[this] def advanceHead(): Unit =
        while (head.isEmpty && itA.hasNext && itB.hasNext) {
          val x = itA.next()
          val y = itB.next()
          head = f(x, y)
        }

      def hasNext: Boolean = {
        advanceHead()
        !head.isEmpty
      }

      def next(): C = {
        advanceHead()
        val res = head getOrElse (throw new NoSuchElementException("next on empty Iterator"))
        head = None
        res
      }
    }

  final def mapToArray[A, B: ClassTag](xs: List[A])(f: A => B): Array[B] = {
    val arr = new Array[B](xs.length)
    var ix = 0
    var ys = xs
    while (ix < arr.length){
      arr(ix) = f(ys.head)
      ix += 1
      ys = ys.tail
    }
    arr
  }

  final def mapFromArray[A, B](arr: Array[A])(f: A => B): List[B] = {
    var ix = arr.length
    var xs: List[B] = Nil
    while (ix > 0){
      ix -= 1
      xs = f(arr(ix)) :: xs
    }
    xs
  }

  // "Opt" suffix or traverse clashes with the various traversers' traverses
  final def sequenceOpt[A](as: List[Option[A]]): Option[List[A]] = traverseOpt(as)(identity)
  final def traverseOpt[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    if (as eq Nil) SomeOfNil else {
      var result: ListBuffer[B] = null
      var curr = as
      while (curr ne Nil) {
        f(curr.head) match {
          case Some(b) =>
            if (result eq null) result = ListBuffer.empty
            result += b
          case None => return None
        }
        curr = curr.tail
      }
      Some(result.toList)
    }

  final def partitionInto[A](xs: List[A], pred: A => Boolean, ayes: ListBuffer[A], nays: ListBuffer[A]): Unit = {
    var ys = xs
    while (!ys.isEmpty) {
      val y = ys.head
      if (pred(y)) ayes.addOne(y) else nays.addOne(y)
      ys = ys.tail
    }
  }

  final def bitSetByPredicate[A](xs: List[A])(pred: A => Boolean): mutable.BitSet = {
    val bs = new mutable.BitSet()
    var ys = xs
    var i: Int = 0
    while (! ys.isEmpty){
      if (pred(ys.head))
        bs.add(i)
      ys = ys.tail
      i += 1
    }
    bs
  }

  final def transposeSafe[A](ass: List[List[A]]): Option[List[List[A]]] = try {
    Some(ass.transpose)
  } catch {
    case _: IllegalArgumentException => None
  }

  /** True if two lists have the same length.  Since calling length on linear sequences
    *  is O(n), it is an inadvisable way to test length equality.
    */
  final def sameLength(xs1: List[_], xs2: List[_]) = compareLengths(xs1, xs2) == 0
  @tailrec final def compareLengths(xs1: List[_], xs2: List[_]): Int =
    if (xs1.isEmpty) { if (xs2.isEmpty) 0 else -1 }
    else if (xs2.isEmpty) 1
    else compareLengths(xs1.tail, xs2.tail)

  /** Again avoiding calling length, but the lengthCompare interface is clunky.
    */
  final def hasLength(xs: List[_], len: Int) = xs.lengthCompare(len) == 0

  @tailrec final def sumSize(xss: List[List[_]], acc: Int): Int =
    if (xss.isEmpty) acc else sumSize(xss.tail, acc + xss.head.size)

  final def fillList[T](n: Int)(t: T): List[T] = {
    var i = 0
    var result: List[T] = Nil
    while (i < n) {
      result = t :: result
      i += 1
    }
    result
  }

  final def mapToArray[A, B](as: List[A], arr: Array[B], i: Int)(f: A => B): Unit = {
    var these = as
    var index = i
    while (!these.isEmpty) {
      arr(index) = f(these.head)
      index += 1
      these = these.tail
    }
  }

  private val TupleOfNil = (Nil, Nil)
  final def partitionConserve[A](as: List[A])(p: A => Boolean): (List[A], List[A]) = {
    if (as.isEmpty) TupleOfNil
    else {
      var b0 = true
      var canConserve = true
      var ys = as
      var ayes: ListBuffer[A] = null
      var nays: ListBuffer[A] = null
      var n = 0
      while (!ys.isEmpty) {
        val y = ys.head
        val b = p(y)
        if (canConserve) {
          if (n == 0) b0 = b
          else if (b != b0) {
            canConserve = false
            ayes = new ListBuffer[A]
            nays = new ListBuffer[A]
            val prefix = if (b0) ayes else nays
            var j = 0
            var zs = as
            while (j < n) {
              prefix += zs.head
              zs = zs.tail
              j += 1
            }
            (if (b) ayes else nays) += y
          }
          n += 1
        } else {
          (if (b) ayes else nays) += y
        }
        ys = ys.tail
      }
      if (canConserve)
        if (b0) (as, Nil) else (Nil, as)
      else
        (ayes.toList, nays.toList)
    }
  }

}

object Collections extends Collections
