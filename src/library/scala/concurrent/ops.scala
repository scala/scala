/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.concurrent


import java.lang.Thread

/** The object <code>ops</code> ...
 *
 *  @author  Martin Odersky, Stepan Koltsov
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
  def future[A](p: => A): () => A = {
    val result = new SyncVar[A]
    spawn { result setWithCatch p }
    () => result.get
  }

  /**
   *  @param xp ...
   *  @param yp ...
   *  @return   ...
   */
  def par[A, B](xp: => A, yp: => B): (A, B) = {
    val y = new SyncVar[B]
    spawn { y setWithCatch yp }
    (xp, y.get)
  }

  /**
   *  @param start ...
   *  @param end   ...
   *  @param p     ...
   */
  def replicate(start: Int, end: Int)(p: Int => Unit) {
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
