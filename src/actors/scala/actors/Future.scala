/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

/**
 * <p>
 *   A Future is a function of arity 0 that returns a value of type Any.
 *   Applying a future blocks the current actor until its value
 *   is available.
 * </p>
 * <p>
 *   A future can be queried to find out whether its value
 *   is already available.
 * </p>
 *
 * @author Philipp Haller
 * @version 0.9.8
 */
abstract class Future[T](val ch: InputChannel[Any]) extends Function0[T] {
  protected var value: Option[T] = None
  def isSet: Boolean
}

/**
 * The <code>Futures</code> object contains methods that operate on Futures.
 *
 * @version 0.9.8
 * @author Philipp Haller
 */
object Futures {

  def future[T](body: => T): Future[T] = {
    case object Eval
    val a = Actor.actor {
      Actor.react {
        case Eval => Actor.reply(body)
      }
    }
    a !! (Eval, { case any => any.asInstanceOf[T] })
  }

  def alarm(t: Long) = future {
    Actor.reactWithin(t) {
      case TIMEOUT => {}
    }
  }

  def awaitEither[a, b](ft1: Future[a], ft2: Future[b]): Any = {
    val FutCh1 = ft1.ch; val FutCh2 = ft2.ch
    Actor.receive {
      case FutCh1 ! arg1 => arg1
      case FutCh2 ! arg2 => arg2
    }
  }

  /**
   * <p>
   *   Awaits all futures returning an option containing a list of replies,
   *   or timeouts returning <code>None</code>.
   * </p>
   * <p>
   *   Note that some of the futures might already have been awaited.
   * </p>
   */
  def awaitAll(timeout: Long, fts: Future[Any]*): List[Option[Any]] = {
    TimerThread.requestTimeout(Actor.self, null, timeout)

    var resultsMap: collection.mutable.Map[Int, Option[Any]] = new collection.mutable.HashMap[Int, Option[Any]]

    var cnt = 0
    val mappedFts = fts.map(ft =>
      Pair({cnt+=1; cnt-1}, ft))

    val unsetFts = mappedFts.filter((p: Pair[Int, Future[Any]]) => {
      if (p._2.isSet) { resultsMap(p._1) = Some(p._2()); false }
      else { resultsMap(p._1) = None; true }
    })

    val partFuns = unsetFts.map((p: Pair[Int, Future[Any]]) => {
      val FutCh = p._2.ch
      val singleCase: PartialFunction[Any, Pair[Int, Any]] = {
        case FutCh ! any => Pair(p._1, any)
      }
      singleCase
    })

    def awaitWith(partFuns: Seq[PartialFunction[Any, Pair[Int, Any]]]) {
      val reaction: PartialFunction[Any, Unit] = new PartialFunction[Any, Unit] {
        def isDefinedAt(msg: Any) = msg match {
          case TIMEOUT => true
          case _ => partFuns exists (_ isDefinedAt msg)
        }
        def apply(msg: Any): Unit = msg match {
          case TIMEOUT => // do nothing
          case _ => {
            val pfOpt = partFuns find (_ isDefinedAt msg)
            val pf = pfOpt.get // succeeds always
            val Pair(idx, subres) = pf(msg)
            resultsMap(idx) = Some(subres)

            val partFunsRest = partFuns filter (_ != pf)
            // wait on rest of partial functions
            if (partFunsRest.length > 0)
              awaitWith(partFunsRest)
          }
        }
      }
      Actor.receive(reaction)
    }

    awaitWith(partFuns)

    var results: List[Option[Any]] = Nil
    val size = resultsMap.size
    for (i <- 0 until size) {
      results = resultsMap(size - i - 1) :: results
    }
    results
  }
}
