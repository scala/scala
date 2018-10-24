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
package collection.parallel

package immutable {
  /** A (parallel) sequence consisting of `length` elements `elem`. Used in the `padTo` method.
   *
   *  @tparam T        type of the elements
   *  @param elem      the element in the repetition
   *  @param length    the length of the collection
   */
  private[parallel] class Repetition[T](elem: T, val length: Int) extends ParSeq[T] {
    self =>

    def apply(idx: Int) = if (0 <= idx && idx < length) elem else throw new IndexOutOfBoundsException("" + idx)
    override def seq: collection.immutable.Seq[T] = new collection.AbstractSeq[T] with collection.immutable.Seq[T] {
        override def length: Int = self.length
        override def apply(idx: Int): T = self.apply(idx)
        override def iterator: Iterator[T] = Iterator.continually(elem).take(length)
        override def par: ParSeq[T] = self
      }
    def update(idx: Int, elem: T) = throw new UnsupportedOperationException

    class ParIterator(var i: Int = 0, val until: Int = length, elem: T = self.elem) extends SeqSplitter[T] {
      def remaining = until - i
      def hasNext = i < until
      def next = { i += 1; elem }
      def dup = new ParIterator(i, until, elem)
      def psplit(sizes: Int*) = {
        val incr = sizes.scanLeft(0)(_ + _)
        for ((start, end) <- incr.init zip incr.tail) yield new ParIterator(i + start, (i + end) min until, elem)
      }
      def split = psplit(remaining / 2, remaining - remaining / 2)
    }

    def splitter = new ParIterator
  }
}

package object immutable {
  /* package level methods */
  def repetition[T](elem: T, len: Int) = new Repetition(elem, len)
}
