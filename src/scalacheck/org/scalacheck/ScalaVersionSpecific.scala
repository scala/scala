/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2018 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck

import rng.Seed

private[scalacheck] object ScalaVersionSpecific {
  def toLazyList[T](i: IterableOnce[T]) = LazyList.from(i)
}

private[scalacheck] trait GenVersionSpecific {

  /** Generates an infinite lazy list. */
  def infiniteLazyList[T](g: => Gen[T]): Gen[LazyList[T]] = {
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): LazyList[A] = f(z) match {
      case Some((h, s)) => h #:: unfold(s)(f)
      case None => LazyList.empty
    }
    Gen.gen { (p, seed0) =>
      new Gen.R[LazyList[T]] {
        val result: Option[LazyList[T]] = Some(unfold(seed0)(s => Some(g.pureApply(p, s) -> s.next)))
        val seed: Seed = seed0.next
      }
    }
  }
}

private[scalacheck] trait GenSpecificationVersionSpecific

private[scalacheck] trait CogenVersionSpecific {
  implicit def cogenLazyList[A: Cogen]: Cogen[LazyList[A]] =
    Cogen.it(_.iterator)
}
