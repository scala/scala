/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection.parallel

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
    override def seq = throw new UnsupportedOperationException
    def update(idx: Int, elem: T) = throw new UnsupportedOperationException

    type SCPI = SignalContextPassingIterator[ParIterator]

    class ParIterator(var i: Int = 0, val until: Int = length, elem: T = self.elem) extends super.ParIterator {
      me: SignalContextPassingIterator[ParIterator] =>

      def remaining = until - i
      def hasNext = i < until
      def next = { i += 1; elem }
      def dup = new ParIterator(i, until, elem) with SCPI
      def psplit(sizes: Int*) = {
        val incr = sizes.scanLeft(0)(_ + _)
        for ((start, end) <- incr.init zip incr.tail) yield new ParIterator(i + start, (i + end) min until, elem) with SCPI
      }
      def split = psplit(remaining / 2, remaining - remaining / 2)
    }

    def splitter = new ParIterator with SCPI
  }
}

package object immutable {
  /* package level methods */
  def repetition[T](elem: T, len: Int) = new Repetition(elem, len)
}
