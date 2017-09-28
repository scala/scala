package strawman.collection
package decorators

class IteratorDecorator[A](`this`: Iterator[A]) {

  def intersperse[B >: A](sep: B): Iterator[B] = new Iterator[B] {
    var intersperseNext = false
    override def hasNext = intersperseNext || `this`.hasNext
    override def next() = {
      val elem = if (intersperseNext) sep else `this`.next()
      intersperseNext = !intersperseNext && `this`.hasNext
      elem
    }
  }

  def intersperse[B >: A](start: B, sep: B, end: B): Iterator[B] = new Iterator[B] {
    var started = false
    var finished = false
    var intersperseNext = false

    override def hasNext: Boolean = !finished || intersperseNext || `this`.hasNext

    override def next(): B =
      if (!started) {
        started = true
        start
      } else if (intersperseNext) {
        intersperseNext = false
        sep
      } else if (`this`.hasNext) {
        val elem = `this`.next()
        intersperseNext = `this`.hasNext
        elem
      } else if (!finished) {
        finished = true
        end
      } else {
        throw new NoSuchElementException("next on empty iterator")
      }
  }

}
