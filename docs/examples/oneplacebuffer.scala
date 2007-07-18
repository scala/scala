package examples

object oneplacebuffer {

  import scala.concurrent.{MailBox, ops}

  class OnePlaceBuffer {
    private val m = new MailBox() {}          // An internal mailbox
    private case class Empty()                // Types of messages we deal with
    private case class Full(x: Int)

    m send Empty()                            // Initialization

    def write(x: Int) {
      m receive {
        case Empty() =>
          println("put " + x)
          m send Full(x)
      }
    }

    def read: Int = m receive {
      case Full(x) =>
        println("get " + x)
        m send Empty(); x
    }
  }

  def kill(delay: Int) = new java.util.Timer().schedule(
    new java.util.TimerTask {
      override def run() = {
        println("[killed]")
        exit(0)
      }
    },
    delay) // in milliseconds

  def main(args: Array[String]) {
    val buf = new OnePlaceBuffer
    val random = new java.util.Random()

    def producer(n: Int) {
      Thread.sleep(random.nextInt(1000))
      buf.write(n)
      producer(n + 1)
    }

    def consumer {
      Thread.sleep(random.nextInt(1000))
      val n = buf.read
      consumer
    }

    ops.spawn(producer(0))
    ops.spawn(consumer)
    kill(10000)
  }

}

