import scala.annotation._
import scala.util.continuations._
import java.util.concurrent.atomic._

@deprecated("Suppress warnings", since="2.11")
object Test {
  def main(args: Array[String]) {
    val map = Map("foo" -> 1, "bar" -> 2)
    reset {
      val mapped =
        for {
          (location, accessors) <- new ContinuationizedParallelIterable(map)
        } yield {
          shiftUnit0[Int, Unit](23)
        }
      println(mapped.toList)
    }
  }

final class ContinuationizedParallelIterable[+A](protected val underline: Iterable[A]) {
  def toList = underline.toList.sortBy(_.toString)

  final def filter(p: A => Boolean @suspendable): ContinuationizedParallelIterable[A] @suspendable =
    shift(
      new AtomicInteger(1) with ((ContinuationizedParallelIterable[A] => Unit) => Unit) {
        private val results = new AtomicReference[List[A]](Nil)

        @tailrec
        private def add(element: A) {
          val old = results.get
          if (!results.compareAndSet(old, element :: old)) {
            add(element)
          }
        }

        override final def apply(continue: ContinuationizedParallelIterable[A] => Unit) {
          for (element <- underline) {
            super.incrementAndGet()
            reset {
              val pass = p(element)
              if (pass) {
                add(element)
              }
              if (super.decrementAndGet() == 0) {
                continue(new ContinuationizedParallelIterable(results.get))
              }
            }
          }
          if (super.decrementAndGet() == 0) {
            continue(new ContinuationizedParallelIterable(results.get))
          }
        }
      })

  final def foreach[U](f: A => U @suspendable): Unit @suspendable =
    shift(
      new AtomicInteger(1) with ((Unit => Unit) => Unit) {
        override final def apply(continue: Unit => Unit) {
          for (element <- underline) {
            super.incrementAndGet()
            reset {
              f(element)
              if (super.decrementAndGet() == 0) {
                continue()
              }
            }
          }
          if (super.decrementAndGet() == 0) {
            continue()
          }
        }
      })

  final def map[B: Manifest](f: A => B @suspendable): ContinuationizedParallelIterable[B] @suspendable =
    shift(
      new AtomicInteger(underline.size) with ((ContinuationizedParallelIterable[B] => Unit) => Unit) {
        override final def apply(continue: ContinuationizedParallelIterable[B] => Unit) {
          val results = new Array[B](super.get)
          for ((element, i) <- underline.view.zipWithIndex) {
            reset {
              val result = f(element)
              results(i) = result
              if (super.decrementAndGet() == 0) {
                continue(new ContinuationizedParallelIterable(results))
              }
            }
          }
        }
      })
}
}
