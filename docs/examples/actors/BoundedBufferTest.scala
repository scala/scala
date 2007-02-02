package examples.actors

import scala.actors.Actor._

object boundedbuffer {
  class BoundedBuffer[T](N: int) {
    private case class Get
    private case class Put(x: T)

    private val buffer = actor {
      val buf = new Array[T](N)
      var in = 0; var out = 0; var n = 0
      while(true) {
        receive {
          case Put(x) if n < N =>
            buf(in) = x; in = (in + 1) % N; n = n + 1; reply()
          case Get() if n > 0 =>
            val r = buf(out); out = (out + 1) % N; n = n - 1; reply(r)
        }
      }
    }

    def put(x: T): Unit = buffer !? Put(x)

    def get: T = (buffer !? Get()).asInstanceOf[T]
  }

  def main(args: Array[String]) = {
    val buf = new BoundedBuffer[Int](1)
    buf.put(42)
    scala.Console.println("" + buf.get)
  }
}
