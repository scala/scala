import scala.actors.Reactor

object Test {
  case class Stop()
  case class Get(from: Reactor)
  case class Put(x: Int)

  class UnboundedBuffer extends Reactor {
    def act() {
      react {
        case Stop() =>
        case Get(from) =>
          val consumer = from
          react {
            case msg @ Put(x) =>
              consumer ! x
              act()
          }
      }
    }
  }

  class Producer(buf: UnboundedBuffer, n: Int, delay: Long, parent: Reactor) extends Reactor {
    def act() {
      var i = 0
      while (i < n) {
        i += 1
        if (delay > 0) Thread.sleep(delay)
        buf ! Put(42)
      }
      parent ! Stop()
    }
  }

  class Consumer(buf: UnboundedBuffer, n: Int, delay: Long, parent: Reactor) extends Reactor {
    val step = n / 10
    var i = 0
    def act() {
      if (i < n) {
        i += 1
        if (delay > 0) Thread.sleep(delay)
        buf ! Get(this)
        react {
          case res =>
            if (i % step == 0)
              println(res)
            act()
        }
      } else {
        parent ! Stop()
      }
    }
  }

  def main(args: Array[String]) {
    val parent = new Reactor {
      def act() {
        val buffer = new UnboundedBuffer
        buffer.start()
        val producer = new Producer(buffer, 10000, 0, this)
        producer.start()
        val consumer = new Consumer(buffer, 10000, 0, this)
        consumer.start()
        react {
          case Stop() =>
            react {
              case Stop() =>
                buffer ! Stop()
            }
        }
      }
    }
    parent.start()
  }
}
