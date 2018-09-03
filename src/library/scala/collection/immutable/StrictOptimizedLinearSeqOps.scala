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

package scala
package collection
package immutable

import scala.annotation.tailrec

trait StrictOptimizedLinearSeqOps[+A, +CC[X] <: LinearSeq[X], +C <: LinearSeq[A] with StrictOptimizedLinearSeqOps[A, CC, C]]
  extends LinearSeqOps[A, CC, C]
    with StrictOptimizedSeqOps[A, CC, C] {

  // A more efficient iterator implementation than the default LinearSeqIterator
  override def iterator: Iterator[A] = new AbstractIterator[A] {
    private[this] var current: collection.Iterable[A] = toIterable
    def hasNext = !current.isEmpty
    def next() = { val r = current.head; current = current.tail; r }
  }

  // Optimized version of `drop` that avoids copying
  override def drop(n: Int): C = {
    @tailrec def loop(n: Int, s: C): C =
      if (n <= 0 || s.isEmpty) s
      else loop(n - 1, s.tail)
    loop(n, coll)
  }

  override def dropWhile(p: A => Boolean): C = {
    @tailrec def loop(s: C): C =
      if (s.nonEmpty && p(s.head)) loop(s.tail)
      else s
    loop(coll)
  }
}
