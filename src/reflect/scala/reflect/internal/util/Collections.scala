/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.reflect.internal.util

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
  final def mforeach[A](xss: List[List[A]])(f: A => Unit) =
    xss foreach (_ foreach f)
  final def mfind[A](xss: List[List[A]])(p: A => Boolean): Option[A] = {
    var res: Option[A] = null
    mforeach(xss)(x => if ((res eq null) && p(x)) res = Some(x))
    if (res eq null) None else res
  }
  final def mfilter[A](xss: List[List[A]])(p: A => Boolean) =
    for (xs <- xss; x <- xs; if p(x)) yield x

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
  final def map3[A, B, C, D](xs1: List[A], xs2: List[B], xs3: List[C])(f: (A, B, C) => D): List[D] = {
    if (xs1.isEmpty || xs2.isEmpty || xs3.isEmpty) Nil
    else f(xs1.head, xs2.head, xs3.head) :: map3(xs1.tail, xs2.tail, xs3.tail)(f)
  }
  final def flatMap2[A, B, C](xs1: List[A], xs2: List[B])(f: (A, B) => List[C]): List[C] = {
    val lb = new ListBuffer[C]
    var ys1 = xs1
    var ys2 = xs2
    while (!ys1.isEmpty && !ys2.isEmpty) {
      lb ++= f(ys1.head, ys2.head)
      ys1 = ys1.tail
      ys2 = ys2.tail
    }
    lb.toList
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
    buf.result
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
  final def forall2[A, B](xs1: List[A], xs2: List[B])(f: (A, B) => Boolean): Boolean = {
    var ys1 = xs1
    var ys2 = xs2
    while (!ys1.isEmpty && !ys2.isEmpty) {
      if (!f(ys1.head, ys2.head))
        return false

      ys1 = ys1.tail
      ys2 = ys2.tail
    }
    true
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

  final def transposeSafe[A](ass: List[List[A]]): Option[List[List[A]]] = try {
    Some(ass.transpose)
  } catch {
    case _: IllegalArgumentException => None
  }
}

object Collections extends Collections { }

