package scala.collection









package object immutable {

  trait RangeUtils[+Repr <: RangeUtils[Repr]] {

    def start: Int
    def end: Int
    def step: Int
    def inclusive: Boolean
    def create(_start: Int, _end: Int, _step: Int, _inclusive: Boolean): Repr

    private final def inclusiveLast: Int = {
      val size = end.toLong - start.toLong
      (size / step.toLong * step.toLong + start.toLong).toInt
    }

    final def _last: Int = if (!inclusive) {
      if (step == 1 || step == -1) end - step
      else {
        val inclast = inclusiveLast
        if ((end.toLong - start.toLong) % step == 0) inclast - step else inclast
      }
    } else {
      if (step == 1 || step == -1) end
      else inclusiveLast
    }

    final def _foreach[U](f: Int => U) = if (_length > 0) {
      var i = start
      val last = _last
      while (i != last) {
        f(i)
        i += step
      }
    }

    final def _length: Int = if (!inclusive) {
      if (end > start == step > 0 && start != end) {
        (_last.toLong - start.toLong) / step.toLong + 1
      } else 0
    }.toInt else {
      if (end > start == step > 0 || start == end) {
        (_last.toLong - start.toLong) / step.toLong + 1
      } else 0
    }.toInt

    final def _apply(idx: Int): Int = {
      if (idx < 0 || idx >= _length) throw new IndexOutOfBoundsException(idx.toString)
      start + idx * step
    }

    private def locationAfterN(n: Int) = if (n > 0) {
      if (step > 0) ((start.toLong + step.toLong * n.toLong) min _last.toLong).toInt
      else ((start.toLong + step.toLong * n.toLong) max _last.toLong).toInt
    } else start

    final def _take(n: Int) = if (n > 0 && _length > 0) {
      create(start, locationAfterN(n), step, true)
    } else create(start, start, step, false)

    final def _drop(n: Int) = create(locationAfterN(n), end, step, inclusive)

    final def _slice(from: Int, until: Int) = _drop(from)._take(until - from)

  }

}





