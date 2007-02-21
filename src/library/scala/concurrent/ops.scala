/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.concurrent


import java.lang.Thread

/** The object <code>ops</code> ...
 *
 *  @author  Martin Odersky
 *  @version 1.0, 12/03/2003
 */
object ops {

  /**
   *  @param p ...
   */
  def spawn(p: => Unit) = {
    val t = new Thread() { override def run() = p }
    t.start()
  }

  /**
   *  @param p ...
   *  @return  ...
   */
  def future[a](p: => a): () => a = {
    val result = new SyncVar[a]
    spawn { result set p }
    () => result.get
  }

  /**
   *  @param xp ...
   *  @param yp ...
   *  @return   ...
   */
  def par[a, b](xp: => a, yp: => b): (a, b) = {
    val y = new SyncVar[b]
    spawn { y set yp }
    (xp, y.get)
  }

  /**
   *  @param start ...
   *  @param end   ...
   *  @param p     ...
   */
  def replicate(start: Int, end: Int)(p: Int => Unit): Unit = {
    if (start == end)
      ()
    else if (start + 1 == end)
      p(start)
    else {
      val mid = (start + end) / 2
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
