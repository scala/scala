package scala.collection.parallel











package object immutable {

  /** A (parallel) sequence consisting of `length` elements `elem`. Used in the `padTo` method.
   *
   *  @tparam T        type of the elements
   *  @param elem      the element in the repetition
   *  @param length    the length of the collection
   */
  private[parallel] class Repetition[T](elem: T, val length: Int) extends ParallelSeq[T] {
    self =>

    def apply(idx: Int) = if (0 <= idx && idx < length) elem else throw new IndexOutOfBoundsException
    def seq = throw new UnsupportedOperationException
    def update(idx: Int, elem: T) = throw new UnsupportedOperationException

    type SCPI = SignalContextPassingIterator[ParallelIterator]

    class ParallelIterator(var i: Int = 0, val until: Int = length, elem: T = self.elem) extends super.ParallelIterator {
      me: SignalContextPassingIterator[ParallelIterator] =>
      def remaining = until - i
      def hasNext = i < until
      def next = { i += 1; elem }
      def psplit(sizes: Int*) = {
        val incr = sizes.scanLeft(0)(_ + _)
        for ((start, end) <- incr.init zip incr.tail) yield new ParallelIterator(i + start, (i + end) min until, elem) with SCPI
      }
      def split = psplit(remaining / 2, remaining - remaining / 2)
    }

    def parallelIterator = new ParallelIterator with SCPI

  }

}










