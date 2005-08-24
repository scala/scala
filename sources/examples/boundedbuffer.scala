package examples;

object boundedbuffer {

  import concurrent.ops._;

  class BoundedBuffer[a](N: Int) {
    var in, out, n = 0;
    val elems = new Array[a](N);

    def await(cond: => Boolean) = while (!cond) { wait() }

    def put(x: a) = synchronized {
      await (n < N);
      elems(in) = x; in = (in + 1) % N; n = n + 1;
      if (n == 1) notifyAll();
    }

    def get: a = synchronized {
      await (n != 0);
      val x = elems(out); out = (out + 1) % N ; n = n - 1;
      if (n == N - 1) notifyAll();
      x
    }
  }

  def main(args: Array[String]) = {
    val buf = new BoundedBuffer[String](10);
    var cnt = 0;
    def produceString = { cnt = cnt + 1; cnt.toString() }
    def consumeString(ss: String) = System.out.println(ss);
    spawn { while (true) { val ssss = produceString; buf.put(ssss) } }
    spawn { while (true) { val s = buf.get; consumeString(s) } }
  }

}
