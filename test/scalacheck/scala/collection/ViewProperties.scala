/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.collection

import org.scalacheck._
import org.scalacheck.Prop._

import scala.collection.mutable.ListBuffer

object ViewProperties extends Properties("View") {

  type Elem = Int
  type SomeSeqOps = SeqOps[Elem, Iterable, Iterable[Elem]]

  private def expectedPatch(seq: SomeSeqOps, from: Int, other: Iterable[Elem], replaced: Int): Seq[Elem] = {
    val (prefix, suffix) = seq.splitAt(from)
    ListBuffer.empty[Elem] ++= prefix ++= other ++= suffix.drop(replaced)
  }

  property("`SeqOps#patch(...)` (i.e. `iterableFactory.from(View.Patched(...))`) correctness") = {
    // we use `mutable.ArraySeq` because it uses the default `patch`
    // implementation, rather than one from `StrictOptimisedSeqOps`
    forAll { (seq: mutable.ArraySeq[Elem], from: Int, other: Iterable[Elem], replaced: Int) =>
      val expected = expectedPatch(seq, from, other, replaced)
      val patchedWithIterable = seq.patch(from, other, replaced)
      val patchedWithIterableOnce = seq.patch(from, other.iterator, replaced)

      // we don't need to use `sameElements` like below, because
      // both `expected` and patched are `Seq` this time
      ((expected =? patchedWithIterable) :| "`patch(_, Iterable, _)` is performed correctly") &&
        ((expected =? patchedWithIterableOnce) :| "`patch(_, IterableOnce, _)` is performed correctly")
    }
  }


  property("`SeqOps#view.patch(...)` (i.e. `View.Patched` used directly) correctness and consistency") =
    forAll { (seq: Seq[Elem], from: Int, other: Iterable[Elem], replaced: Int) =>
      val expected = expectedPatch(seq, from, other, replaced)
      val patchedWithIterable = seq.view.patch(from, other, replaced)
      val patchedWithIterableOnce = seq.view.patch(from, other.iterator, replaced)

      (expected.sameElements(patchedWithIterable) :| "`patch(_, Iterable, _)` is performed correctly") &&
        (expected.sameElements(patchedWithIterable) :| "`view.patch(_, Iterable, _)` remains the same after multiple iterations") &&
        (expected.sameElements(patchedWithIterableOnce) :| "`patch(_, IterableOnce, _)` is performed correctly") &&
        (expected.sameElements(patchedWithIterableOnce) :| "`view.patch(_, IterableOnce, _)` remains the same after multiple iterations")
    }
}
