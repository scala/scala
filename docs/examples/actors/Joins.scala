package examples.actors

import scala.actors.Actor._

abstract class Producer[T] extends Iterator[T] {

  protected def produce(x: T): unit = coordinator !? HasValue(x)
  protected def produceValues: unit

  def hasNext: boolean = { setCurrent(); !current.isEmpty }
  def next: T = {
    setCurrent()
    val res = current.get
    current = (coordinator !? Next).asInstanceOf[Option[T]]
    res
  }

  private var current: Option[T] = null
  private def setCurrent() = if (current == null) current = (coordinator !? Next).asInstanceOf[Option[T]]

  private case class  HasValue(value: T)
  private case object Next
  private case object Done
  private case object Continue

  private val coordinator = actor {
    while (true) {
      receive {
        case Next =>
          reply {
            receive {
              case HasValue(v) =>
                reply()
                Some(v)
              case Done =>
                None
            }
          }
      }
    }
  }

  actor {
    produceValues
    coordinator !? Done
    ()
  }
}

object Joins extends Application {
  def from(m: int, n: int) = new Producer[int] {
    def produceValues = for (val i <- m until n) produce(i)
  }

  // note that it works from the main thread
  val it = from(1, 10)
  while (it.hasNext) {
    Console.println(it.next)
  }
}
