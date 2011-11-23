package examples

object oneplacebuffer {

  import scala.actors.Actor._
  import scala.concurrent.ops

  class OnePlaceBuffer {
    private case class Put(x: Int)
    private case object Get

    private val m = actor {
      var buf: Option[Int] = None
      loop {
        react {
          case Put(x) if buf.isEmpty =>
            println("put "+x); 
            buf = Some(x); reply()
          case Get if !buf.isEmpty =>
            val x = buf.get
            println("get "+x)
            buf = None; reply(x)
        }
      }
    }
    m.start()

    def write(x: Int) { m !? Put(x) }

    def read(): Int = (m !? Get).asInstanceOf[Int]
  }

  def kill(delay: Int) = new java.util.Timer().schedule(
    new java.util.TimerTask {
      override def run() {
        println("[killed]")
        sys exit 0
      }
    },
    delay) // in milliseconds

  def main(args: Array[String]) {
    val buf = new OnePlaceBuffer
    val random = new java.util.Random()

    def producer(n: Int) {
      Thread.sleep(random nextInt 1000)
      buf write n
      producer(n + 1)
    }

    def consumer {
      Thread.sleep(random nextInt 1000)
      val n = buf.read()
      consumer
    }

    ops spawn producer(0)
    ops spawn consumer
    kill(10000)
  }

}

