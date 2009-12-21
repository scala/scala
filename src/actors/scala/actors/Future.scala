/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

/** A `Future[T]` is a function of arity 0 that returns
 *  a value of type `T`.
 *  Applying a future blocks the current actor (`Actor.self`)
 *  until the future's value is available.
 *
 *  A future can be queried to find out whether its value
 *  is already available without blocking.
 *
 *  @author Philipp Haller
 */
abstract class Future[+T](val inputChannel: InputChannel[T]) extends Responder[T] with Function0[T] {
  private[actors] var fvalue: Option[Any] = None
  private[actors] def fvalueTyped = fvalue.get.asInstanceOf[T]

  @deprecated("this member is going to be removed in a future release")
  protected def value: Option[Any] = fvalue
  @deprecated("this member is going to be removed in a future release")
  protected def value_=(x: Option[Any]) { fvalue = x }

  /** Tests whether the future's result is available.
   *
   *  @return `true`  if the future's result is available,
   *          `false` otherwise.
   */
  def isSet: Boolean
}

/** The <code>Futures</code> object contains methods that operate on futures.
 *
 *  @author Philipp Haller
 */
object Futures {

  private case object Eval

  /** Arranges for the asynchronous execution of `body`,
   *  returning a future representing the result.
   *
   *  @param  body the computation to be carried out asynchronously
   *  @return      the future representing the result of the
   *               computation
   */
  def future[T](body: => T): Future[T] = {
    val a = new DaemonActor {
      def act() {
        Actor.react {
          case Eval => Actor.reply(body)
        }
      }
    }
    a.start()
    a !! (Eval, { case any => any.asInstanceOf[T] })
  }

  /** Creates a future that resolves after a given time span.
   *
   *  @param  timespan the time span in ms after which the future resolves
   *  @return          the future
   */
  def alarm(timespan: Long) = future {
    Actor.reactWithin(timespan) {
      case TIMEOUT => {}
    }
  }

  /** Waits for the first result returned by one of two
   *  given futures.
   *
   *  @param  ft1 the first future
   *  @param  ft2 the second future
   *  @return the result of the future that resolves first
   */
  def awaitEither[A, B >: A](ft1: Future[A], ft2: Future[B]): B = {
    val FutCh1 = ft1.inputChannel
    val FutCh2 = ft2.inputChannel
    Actor.receive {
      case FutCh1 ! arg1 => arg1.asInstanceOf[B]
      case FutCh2 ! arg2 => arg2.asInstanceOf[B]
    }
  }

  /** Waits until either all futures are resolved or a given
   *  time span has passed. Results are collected in a list of
   *  options. The result of a future that resolved during the
   *  time span is its value wrapped in `Some`. The result of a
   *  future that did not resolve during the time span is `None`.
   *
   *  Note that some of the futures might already have been awaited,
   *  in which case their value is returned wrapped in `Some`.
   *  Passing a timeout of 0 causes `awaitAll` to return immediately.
   *
   *  @param  timeout the time span in ms after which waiting is
   *                  aborted
   *  @param  fts     the futures to be awaited
   *  @return         the list of optional future values
   *  @throws java.lang.IllegalArgumentException  if timeout is negative,
   *                  or timeout + `System.currentTimeMillis()` is negative.
   */
  def awaitAll(timeout: Long, fts: Future[Any]*): List[Option[Any]] = {
    var resultsMap: collection.mutable.Map[Int, Option[Any]] = new collection.mutable.HashMap[Int, Option[Any]]

    var cnt = 0
    val mappedFts = fts.map(ft =>
      Pair({cnt+=1; cnt-1}, ft))

    val unsetFts = mappedFts.filter((p: Pair[Int, Future[Any]]) => {
      if (p._2.isSet) { resultsMap(p._1) = Some(p._2()); false }
      else { resultsMap(p._1) = None; true }
    })

    val partFuns = unsetFts.map((p: Pair[Int, Future[Any]]) => {
      val FutCh = p._2.inputChannel
      val singleCase: PartialFunction[Any, Pair[Int, Any]] = {
        case FutCh ! any => Pair(p._1, any)
      }
      singleCase
    })

    val thisActor = Actor.self
    val timerTask = new java.util.TimerTask {
      def run() { thisActor ! TIMEOUT }
    }
    Actor.timer.schedule(timerTask, timeout)

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

    if (partFuns.length > 0)
      awaitWith(partFuns)

    var results: List[Option[Any]] = Nil
    val size = resultsMap.size
    for (i <- 0 until size) {
      results = resultsMap(size - i - 1) :: results
    }

    // cancel scheduled timer task
    timerTask.cancel()

    results
  }

  private[actors] def fromInputChannel[T](inputChannel: InputChannel[T]): Future[T] =
    new Future[T](inputChannel) {
      def apply() =
        if (isSet) fvalueTyped
        else inputChannel.receive {
          case any => fvalue = Some(any); fvalueTyped
        }
      def respond(k: T => Unit): Unit =
        if (isSet) k(fvalueTyped)
        else inputChannel.react {
          case any => fvalue = Some(any); k(fvalueTyped)
        }
      def isSet = fvalue match {
        case None => inputChannel.receiveWithin(0) {
          case TIMEOUT => false
          case any => fvalue = Some(any); true
        }
        case Some(_) => true
      }
    }

}
