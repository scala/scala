package examples;

import concurrent.ops._;

class BoundedBuffer[a](N: Int) extends Monitor() with {
  var in = 0, out = 0, n = 0;
  val elems = new Array[a](N);

  def put(x: a) = synchronized {
    await (n < N);
    elems(in) = x ; in = (in + 1) % N ; n = n + 1;
    if (n == 1) notifyAll();
  }

  def get: a = synchronized {
    await (n != 0);
    val x = elems(out) ; out = (out + 1) % N ; n = n - 1;
    if (n == N - 1) notifyAll();
    x
  }
}

module test {
  val buf = new BoundedBuffer[String](10);
  var cnt = 0;
  def produceString = { cnt = cnt + 1; cnt.toString() }
  def consumeString(ss: String) = System.out.println(ss);
  spawn { while (True) { val ssss = produceString ; buf.put(ssss) } }
  spawn { while (True) { val s = buf.get ; consumeString(s) } }
}