/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala
package reflect.internal.util

import scala.collection.{ mutable, immutable }
import scala.annotation.tailrec
import mutable.ListBuffer

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
  final def mforeach[A](xss: Traversable[Traversable[A]])(f: A => Unit) = xss foreach (_ foreach f)

  /** A version of List#map, specialized for List, and optimized to avoid allocation if `as` is empty */
  final def mapList[A, B](as: List[A])(f: A => B): List[B] = if (as eq Nil) Nil else {
    val head = new ::[B](f(as.head), Nil)
    var tail: ::[B] = head
    var rest = as.tail
    while (rest ne Nil) {
      val next = new ::(f(rest.head), Nil)
      tail.tl = next
      tail = next
      rest = rest.tail
    }
    head
  }

  final def collectFirst[A, B](as: List[A])(pf: PartialFunction[A, B]): Option[B] = {
    @tailrec
    def loop(rest: List[A]): Option[B] = rest match {
      case Nil => None
      case a :: as if pf.isDefinedAt(a) => Some(pf(a))
      case a :: as => loop(as)
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
    loop(null, xs, xs, ys)
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
    if (lb eq null) Nil else lb.result
  }

  final def flatCollect[A, B](elems: List[A])(pf: PartialFunction[A, Traversable[B]]): List[B] = {
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

  final def foreachWithIndex[A, B](xs: List[A])(f: (A, Int) => Unit) {
    var index = 0
    var ys = xs
    while (!ys.isEmpty) {
      f(ys.head, index)
      ys = ys.tail
      index += 1
    }
  }

  // @inline
  final def findOrElse[A](xs: TraversableOnce[A])(p: A => Boolean)(orElse: => A): A = {
    xs find p getOrElse orElse
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

  final def sequence[A](as: List[Option[A]]): Option[List[A]] = {
    if (as.exists (_.isEmpty)) None
    else Some(as.flatten)
  }

  final def transposeSafe[A](ass: List[List[A]]): Option[List[List[A]]] = try {
    Some(ass.transpose)
  } catch {
    case _: IllegalArgumentException => None
  }
}

object Collections extends Collections
