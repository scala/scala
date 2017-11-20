package strawman.collection
package decorators

import scala.annotation.tailrec

class IteratorDecorator[A](val `this`: Iterator[A]) extends AnyVal {

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

  def foldSomeLeft[B](z: B)(op: (B, A) => Option[B]): B = {
    var result: B = z
    while (`this`.hasNext) {
      op(result, `this`.next()) match {
        case Some(v) => result = v
        case None => return result
      }
    }
    result
  }

  def lazyFoldRight[B](z: B)(op: A => Either[B, B => B]): B = {

    def chainEval(x: B, fs: immutable.List[B => B]): B =
      fs.foldLeft(x)((x, f) => f(x))

    @tailrec
    def loop(fs: immutable.List[B => B]): B = {
      if (`this`.hasNext) {
        op(`this`.next()) match {
          case Left(v) => chainEval(v, fs)
          case Right(g) => loop(g :: fs)
        }
      } else {
        chainEval(z, fs)
      }
    }

    loop(immutable.List.empty)
  }

}
