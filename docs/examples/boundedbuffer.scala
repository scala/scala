package examples

object boundedbuffer {

  import concurrent.ops._

  class BoundedBuffer[A](N: Int)(implicit m: ClassManifest[A]) {
    var in, out, n = 0
    val elems = new Array[A](N)

    def await(cond: => Boolean) = while (!cond) { wait() }

    def put(x: A) = synchronized {
      await (n < N)
      elems(in) = x; in = (in + 1) % N; n += 1
      if (n == 1) notifyAll()
    }

    def get: A = synchronized {
      await (n != 0)
      val x = elems(out); out = (out + 1) % N ; n -= 1
      if (n == N - 1) notifyAll()
      x
    }
  }

  def kill(delay: Int) = new java.util.Timer().schedule(
    new java.util.TimerTask {
      override def run() = {
        println("[killed]")
        System.exit(0)
      }
    },
    delay) // in milliseconds

  def main(args: Array[String]) {
    val buf = new BoundedBuffer[String](10)
    var cnt = 0
    def produceString = { cnt += 1; cnt.toString() }
    def consumeString(ss: String) = println(ss)
    spawn { while (true) { val ssss = produceString; buf.put(ssss) } }
    spawn { while (true) { val s = buf.get; consumeString(s) } }
    kill(1000)
  }

}
