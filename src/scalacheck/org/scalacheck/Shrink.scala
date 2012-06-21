/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2011 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck

import util.Buildable
import scala.collection.{ JavaConversions => jcl }

sealed abstract class Shrink[T] {
  def shrink(x: T): Stream[T]
}

object Shrink {

  import Stream.{cons, empty}
  import scala.collection._
  import java.util.ArrayList

  /** Interleaves to streams */
  private def interleave[T](xs: Stream[T], ys: Stream[T]): Stream[T] =
    if(xs.isEmpty) ys
    else if(ys.isEmpty) xs
    else Stream(xs.head, ys.head) append interleave(xs.tail, ys.tail)

  /** Shrink instance factory */
  def apply[T](s: T => Stream[T]): Shrink[T] = new Shrink[T] {
    override def shrink(x: T) = s(x)
  }

  /** Shrink a value */
  def shrink[T](x: T)(implicit s: Shrink[T]): Stream[T] = s.shrink(x)

  /** Default shrink instance */
  implicit def shrinkAny[T]: Shrink[T] = Shrink(x => empty)

  /** Shrink instance of container */
  implicit def shrinkContainer[C[_],T](implicit v: C[T] => Traversable[T], s: Shrink[T],
    b: Buildable[T,C]
  ): Shrink[C[T]] = Shrink { xs: C[T] =>

    def removeChunks(n: Int, xs: Stream[T]): Stream[Stream[T]] =
      if(xs.isEmpty) empty
      else if(xs.tail.isEmpty) cons(empty, empty)
      else {
        val n1 = n / 2
        val n2 = n - n1
        lazy val xs1 = xs.take(n1)
        lazy val xs2 = xs.drop(n1)
        lazy val xs3 =
          for(ys1 <- removeChunks(n1,xs1) if !ys1.isEmpty) yield ys1 append xs2
        lazy val xs4 =
          for(ys2 <- removeChunks(n2,xs2) if !ys2.isEmpty) yield xs1 append ys2

        cons(xs1, cons(xs2, interleave(xs3,xs4)))
      }

    def shrinkOne(zs: Stream[T]): Stream[Stream[T]] =
      if(zs.isEmpty) empty
      else {
        val x = zs.head
        val xs = zs.tail
        (for(y <- shrink(x)) yield cons(y,xs)) append
        (for(ys <- shrinkOne(xs)) yield cons(x,ys))
      }

    val ys = v(xs)
    val zs = ys.toStream
    removeChunks(ys.size,zs).append(shrinkOne(zs)).map(b.fromIterable)

  }

  /** Shrink instance of integer */
  implicit lazy val shrinkInt: Shrink[Int] = Shrink { n =>

    def halfs(n: Int): Stream[Int] =
      if(n == 0) empty else cons(n, halfs(n/2))

    if(n == 0) empty else {
      val ns = halfs(n/2).map(n - _)
      cons(0, interleave(ns, ns.map(-1 * _)))
    }
  }

  /** Shrink instance of String */
  implicit lazy val shrinkString: Shrink[String] = Shrink { s =>
    shrinkContainer[List,Char].shrink(s.toList).map(_.mkString)
  }

  /** Shrink instance of Option */
  implicit def shrinkOption[T](implicit s: Shrink[T]): Shrink[Option[T]] =
    Shrink {
      case None    => empty
      case Some(x) => cons(None, for(y <- shrink(x)) yield Some(y))
    }

  /** Shrink instance of 2-tuple */
  implicit def shrinkTuple2[T1,T2](implicit
    s1: Shrink[T1], s2: Shrink[T2]
  ): Shrink[(T1,T2)] =
    Shrink { case (t1,t2) =>
      (for(x1 <- shrink(t1)) yield (x1, t2)) append
      (for(x2 <- shrink(t2)) yield (t1, x2))
    }

  /** Shrink instance of 3-tuple */
  implicit def shrinkTuple3[T1,T2,T3](implicit
    s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3]
  ): Shrink[(T1,T2,T3)] =
    Shrink { case (t1,t2,t3) =>
      (for(x1 <- shrink(t1)) yield (x1, t2, t3)) append
      (for(x2 <- shrink(t2)) yield (t1, x2, t3)) append
      (for(x3 <- shrink(t3)) yield (t1, t2, x3))
    }

  /** Shrink instance of 4-tuple */
  implicit def shrinkTuple4[T1,T2,T3,T4](implicit
    s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4]
  ): Shrink[(T1,T2,T3,T4)] =
    Shrink { case (t1,t2,t3,t4) =>
      (for(x1 <- shrink(t1)) yield (x1, t2, t3, t4)) append
      (for(x2 <- shrink(t2)) yield (t1, x2, t3, t4)) append
      (for(x3 <- shrink(t3)) yield (t1, t2, x3, t4)) append
      (for(x4 <- shrink(t4)) yield (t1, t2, t3, x4))
    }

  /** Shrink instance of 5-tuple */
  implicit def shrinkTuple5[T1,T2,T3,T4,T5](implicit
    s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4],
    s5: Shrink[T5]
  ): Shrink[(T1,T2,T3,T4,T5)] =
    Shrink { case (t1,t2,t3,t4,t5) =>
      (for(x1 <- shrink(t1)) yield (x1, t2, t3, t4, t5)) append
      (for(x2 <- shrink(t2)) yield (t1, x2, t3, t4, t5)) append
      (for(x3 <- shrink(t3)) yield (t1, t2, x3, t4, t5)) append
      (for(x4 <- shrink(t4)) yield (t1, t2, t3, x4, t5)) append
      (for(x5 <- shrink(t5)) yield (t1, t2, t3, t4, x5))
    }

  /** Shrink instance of 6-tuple */
  implicit def shrinkTuple6[T1,T2,T3,T4,T5,T6](implicit
    s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4],
    s5: Shrink[T5], s6: Shrink[T6]
  ): Shrink[(T1,T2,T3,T4,T5,T6)] =
    Shrink { case (t1,t2,t3,t4,t5,t6) =>
      (for(x1 <- shrink(t1)) yield (x1, t2, t3, t4, t5, t6)) append
      (for(x2 <- shrink(t2)) yield (t1, x2, t3, t4, t5, t6)) append
      (for(x3 <- shrink(t3)) yield (t1, t2, x3, t4, t5, t6)) append
      (for(x4 <- shrink(t4)) yield (t1, t2, t3, x4, t5, t6)) append
      (for(x5 <- shrink(t5)) yield (t1, t2, t3, t4, x5, t6)) append
      (for(x6 <- shrink(t6)) yield (t1, t2, t3, t4, t5, x6))
    }

  /** Shrink instance of 7-tuple */
  implicit def shrinkTuple7[T1,T2,T3,T4,T5,T6,T7](implicit
    s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4],
    s5: Shrink[T5], s6: Shrink[T6], s7: Shrink[T7]
  ): Shrink[(T1,T2,T3,T4,T5,T6,T7)] =
    Shrink { case (t1,t2,t3,t4,t5,t6,t7) =>
      (for(x1 <- shrink(t1)) yield (x1, t2, t3, t4, t5, t6, t7)) append
      (for(x2 <- shrink(t2)) yield (t1, x2, t3, t4, t5, t6, t7)) append
      (for(x3 <- shrink(t3)) yield (t1, t2, x3, t4, t5, t6, t7)) append
      (for(x4 <- shrink(t4)) yield (t1, t2, t3, x4, t5, t6, t7)) append
      (for(x5 <- shrink(t5)) yield (t1, t2, t3, t4, x5, t6, t7)) append
      (for(x6 <- shrink(t6)) yield (t1, t2, t3, t4, t5, x6, t7)) append
      (for(x7 <- shrink(t7)) yield (t1, t2, t3, t4, t5, t6, x7))
    }

  /** Shrink instance of 8-tuple */
  implicit def shrinkTuple8[T1,T2,T3,T4,T5,T6,T7,T8](implicit
    s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4],
    s5: Shrink[T5], s6: Shrink[T6], s7: Shrink[T7], s8: Shrink[T8]
  ): Shrink[(T1,T2,T3,T4,T5,T6,T7,T8)] =
    Shrink { case (t1,t2,t3,t4,t5,t6,t7,t8) =>
      (for(x1 <- shrink(t1)) yield (x1, t2, t3, t4, t5, t6, t7, t8)) append
      (for(x2 <- shrink(t2)) yield (t1, x2, t3, t4, t5, t6, t7, t8)) append
      (for(x3 <- shrink(t3)) yield (t1, t2, x3, t4, t5, t6, t7, t8)) append
      (for(x4 <- shrink(t4)) yield (t1, t2, t3, x4, t5, t6, t7, t8)) append
      (for(x5 <- shrink(t5)) yield (t1, t2, t3, t4, x5, t6, t7, t8)) append
      (for(x6 <- shrink(t6)) yield (t1, t2, t3, t4, t5, x6, t7, t8)) append
      (for(x7 <- shrink(t7)) yield (t1, t2, t3, t4, t5, t6, x7, t8)) append
      (for(x8 <- shrink(t8)) yield (t1, t2, t3, t4, t5, t6, t7, x8))
    }

  /** Shrink instance of 9-tuple */
  implicit def shrinkTuple9[T1,T2,T3,T4,T5,T6,T7,T8,T9](implicit
    s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4],
    s5: Shrink[T5], s6: Shrink[T6], s7: Shrink[T7], s8: Shrink[T8],
    s9: Shrink[T9]
  ): Shrink[(T1,T2,T3,T4,T5,T6,T7,T8,T9)] =
    Shrink { case (t1,t2,t3,t4,t5,t6,t7,t8,t9) =>
      (for(x1 <- shrink(t1)) yield (x1, t2, t3, t4, t5, t6, t7, t8, t9)) append
      (for(x2 <- shrink(t2)) yield (t1, x2, t3, t4, t5, t6, t7, t8, t9)) append
      (for(x3 <- shrink(t3)) yield (t1, t2, x3, t4, t5, t6, t7, t8, t9)) append
      (for(x4 <- shrink(t4)) yield (t1, t2, t3, x4, t5, t6, t7, t8, t9)) append
      (for(x5 <- shrink(t5)) yield (t1, t2, t3, t4, x5, t6, t7, t8, t9)) append
      (for(x6 <- shrink(t6)) yield (t1, t2, t3, t4, t5, x6, t7, t8, t9)) append
      (for(x7 <- shrink(t7)) yield (t1, t2, t3, t4, t5, t6, x7, t8, t9)) append
      (for(x8 <- shrink(t8)) yield (t1, t2, t3, t4, t5, t6, t7, x8, t9)) append
      (for(x9 <- shrink(t9)) yield (t1, t2, t3, t4, t5, t6, t7, t8, x9))
    }

}
