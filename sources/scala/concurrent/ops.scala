package scala.concurrent;

object ops {

  def spawn(def p: Unit) = {
    val t = new Thread { override def run() = p; }
    t.run()
  }

  def future[a](def p: a): () => a = {
    val result = new SyncVar[a];
    spawn { result set p }
    () => result.get
  }

  def par[a, b](def xp: a, def yp: b): Pair[a, b] = {
    val y = new SyncVar[b];
    spawn { y set yp }
    Pair(xp, y.get)
  }

  def replicate(start: Int, end: Int)(def p: Int => Unit): Unit = {
    if (start == end) {
    } else if (start + 1 == end) {
      p(start)
    } else {
      val mid = (start + end) / 2;
      spawn { replicate(start, mid)(p) }
      replicate(mid, end)(p)
    }
  }

  def parMap[a,b](f: a => b, xs: Array[a]): Array[b] = {
    val results = new Array[b](xs.length);
    replicate(0, xs.length) { i => results(i) = f(xs(i)) }
    results
  }
}

