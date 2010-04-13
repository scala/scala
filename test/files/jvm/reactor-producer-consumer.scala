import scala.actors.Reactor

object Test {
  case class Stop()
  case class Get(from: Reactor[Any])
  case class Put(x: Int)

  class UnboundedBuffer extends Reactor[Any] {
    def act() {
      try {
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
      } catch {
        case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
          e.printStackTrace()
      }
    }
  }

  class Producer(buf: UnboundedBuffer, n: Int, delay: Long, parent: Reactor[Any]) extends Reactor[Any] {
    def act() {
      try {
      var i = 0
      while (i < n) {
        i += 1
        if (delay > 0) Thread.sleep(delay)
        buf ! Put(42)
      }
      parent ! Stop()
      } catch {
        case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
          e.printStackTrace()
      }
    }
  }

  class Consumer(buf: UnboundedBuffer, n: Int, delay: Long, parent: Reactor[Any]) extends Reactor[Any] {
    val step = n / 10
    var i = 0
    def act() {
      try {
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
      } catch {
        case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
          e.printStackTrace()
      }
    }
  }

  def main(args: Array[String]) {
    val parent = new Reactor[Any] {
      def act() {
        try {
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
        } catch {
          case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
            e.printStackTrace()
        }
      }
    }
    parent.start()
  }
}
