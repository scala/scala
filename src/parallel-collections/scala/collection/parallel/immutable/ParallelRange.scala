package scala.collection.parallel.immutable



import scala.collection.immutable.Range
import scala.collection.immutable.RangeUtils
import scala.collection.parallel.ParallelSeq
import scala.collection.parallel.Combiner
import scala.collection.generic.CanCombineFrom



class ParallelRange(val start: Int, val end: Int, val step: Int, val inclusive: Boolean)
extends ParallelSeq[Int]
   with RangeUtils[ParallelRange] {
  self =>

  def seq = new Range(start, end, step)

  def length = _length

  def apply(idx: Int) = _apply(idx)

  def create(_start: Int, _end: Int, _step: Int, _inclusive: Boolean) = new ParallelRange(_start, _end, _step, _inclusive)

  def parallelIterator = new ParallelRangeIterator with SCPI

  override def toString = seq.toString // TODO

  type SCPI = SignalContextPassingIterator[ParallelRangeIterator]

  class ParallelRangeIterator
  (var start: Int = self.start, val end: Int = self.end, val step: Int = self.step, val inclusive: Boolean = self.inclusive)
  extends ParallelIterator with RangeUtils[ParallelRangeIterator] {
    me: SignalContextPassingIterator[ParallelRangeIterator] =>
    def remaining = _length
    def next = { val r = start; start += step; r }
    def hasNext = remaining > 0
    def split: Seq[ParallelIterator] = psplit(remaining / 2, remaining - remaining / 2)
    def psplit(sizes: Int*): Seq[ParallelIterator] = {
      val incr = sizes.scanLeft(0)(_ + _)
      for ((from, until) <- incr.init zip incr.tail) yield _slice(from, until)
    }
    def create(_start: Int, _end: Int, _step: Int, _inclusive: Boolean) = {
      new ParallelRangeIterator(_start, _end, _step, _inclusive) with SCPI
    }

    override def toString = "ParallelRangeIterator(" + start + ", " + end + ", " + step + ", incl: " + inclusive + ")"

    /* accessors */

    override def foreach[U](f: Int => U): Unit = {
      _foreach(f)
      start = end + step
    }

    override def reduce[U >: Int](op: (U, U) => U): U = {
      var sum = next
      for (elem <- this) sum += elem
      sum
    }

    /* transformers */

    override def map2combiner[S, That](f: Int => S, cb: Combiner[S, That]): Combiner[S, That] = {
      //val cb = pbf(self.repr)
      val sz = remaining
      cb.sizeHint(sz)
      if (sz > 0) {
        val last = _last
        while (start != last) {
          f(start)
          start += step
        }
      }
      cb
    }

  }

}







