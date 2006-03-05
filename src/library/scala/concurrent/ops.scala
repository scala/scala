/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.concurrent;


object ops {

  def spawn(p: => Unit) = {
    val t = new Thread() { override def run() = p; }
    t.start()
  }

  def future[a](p: => a): () => a = {
    val result = new SyncVar[a];
    spawn { result set p }
    () => result.get
  }

  def par[a, b](xp: => a, yp: => b): Pair[a, b] = {
    val y = new SyncVar[b];
    spawn { y set yp }
    Pair(xp, y.get)
  }

  def replicate(start: Int, end: Int)(p: Int => Unit): Unit = {
    if (start == end)
      ()
    else if (start + 1 == end)
      p(start)
    else {
      val mid = (start + end) / 2;
      spawn { replicate(start, mid)(p) }
      replicate(mid, end)(p)
    }
  }

/*
  def parMap[a,b](f: a => b, xs: Array[a]): Array[b] = {
    val results = new Array[b](xs.length);
    replicate(0, xs.length) { i => results(i) = f(xs(i)) }
    results
  }
*/

}
