/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.reflect.internal.util

import scala.collection.{ mutable, immutable }
import scala.annotation.tailrec
import mutable.ListBuffer

/** Profiler driven changes.
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
}
