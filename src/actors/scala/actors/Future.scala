/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.actors

import scala.actors.scheduler.DaemonScheduler
import scala.concurrent.SyncVar

/** A function of arity 0, returning a value of type `T` that,
 *  when applied, blocks the current actor (`Actor.self`)
 *  until the future's value is available.
 *
 *  A future can be queried to find out whether its value
 *  is already available without blocking.
 *
 *  @author Philipp Haller
 */
@deprecated("Use the scala.concurrent.Future instead. For migration from the scala.actors package refer to the Actors Migration Guide.", "2.11.0")
abstract class Future[+T] extends Responder[T] with Function0[T] {

  @volatile
  private[actors] var fvalue: Option[Any] = None
  private[actors] def fvalueTyped = fvalue.get.asInstanceOf[T]

  /** Tests whether the future's result is available.
   *
   *  @return `true`  if the future's result is available,
   *          `false` otherwise.
   */
  def isSet: Boolean

  /** Returns an input channel that can be used to receive the future's result.
   *
   *  @return the future's input channel
   */
  def inputChannel: InputChannel[T]

}

private case object Eval

private class FutureActor[T](fun: SyncVar[T] => Unit, channel: Channel[T]) extends Future[T] with DaemonActor {

  var enableChannel = false // guarded by this

  def isSet = !fvalue.isEmpty

  def apply(): T = {
    if (fvalue.isEmpty) {
      this !? Eval
    }
    fvalueTyped
  }

  def respond(k: T => Unit) {
    if (isSet) k(fvalueTyped)
    else {
      val ft = this !! Eval
      ft.inputChannel.react {
        case _ => k(fvalueTyped)
      }
    }
  }

  def inputChannel: InputChannel[T] = {
    synchronized {
      if (!enableChannel) {
        if (isSet)
          channel ! fvalueTyped
        enableChannel = true
      }
    }
    channel
  }

  def act() {
    val res = new SyncVar[T]

    {
      fun(res)
    } andThen {

      synchronized {
        val v = res.get
        fvalue =  Some(v)
        if (enableChannel)
          channel ! v
      }

      loop {
        react {
          // This is calling ReplyReactor#reply(msg: Any).
          // Was: reply().  Now: reply(()).
          case Eval => reply(())
        }
      }
    }
  }
}

/** Methods that operate on futures.
 *
 *  @author Philipp Haller
 */
@deprecated("Use the object scala.concurrent.Future instead. For migration from the scala.actors package refer to the Actors Migration Guide.", "2.11.0")
object Futures {

  /** Arranges for the asynchronous execution of `body`,
   *  returning a future representing the result.
   *
   *  @param  body the computation to be carried out asynchronously
   *  @return      the future representing the result of the
   *               computation
   */
  def future[T](body: => T): Future[T] = {
    val c = new Channel[T](Actor.self(DaemonScheduler))
    val a = new FutureActor[T](_.set(body), c)
    a.start()
    a
  }

  /** Creates a future that resolves after a given time span.
   *
   *  @param  timespan the time span in ms after which the future resolves
   *  @return          the future
   */
  def alarm(timespan: Long): Future[Unit] = {
    val c = new Channel[Unit](Actor.self(DaemonScheduler))
    val fun = (res: SyncVar[Unit]) => {
      Actor.reactWithin(timespan) {
        case TIMEOUT => res.set({})
      }
    }
    val a = new FutureActor[Unit](fun, c)
    a.start()
    a
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
    val resultsMap: scala.collection.mutable.Map[Int, Option[Any]] = new scala.collection.mutable.HashMap[Int, Option[Any]]

    var cnt = 0
    val mappedFts = fts.map(ft =>
      ({cnt+=1; cnt-1}, ft))

    val unsetFts = mappedFts.filter((p: Tuple2[Int, Future[Any]]) => {
      if (p._2.isSet) { resultsMap(p._1) = Some(p._2()); false }
      else { resultsMap(p._1) = None; true }
    })

    val partFuns = unsetFts.map((p: Tuple2[Int, Future[Any]]) => {
      val FutCh = p._2.inputChannel
      val singleCase: PartialFunction[Any, Tuple2[Int, Any]] = {
        case FutCh ! any => (p._1, any)
      }
      singleCase
    })

    val thisActor = Actor.self
    val timerTask = new java.util.TimerTask {
      def run() { thisActor ! TIMEOUT }
    }
    Actor.timer.schedule(timerTask, timeout)

    def awaitWith(partFuns: Seq[PartialFunction[Any, Tuple2[Int, Any]]]) {
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
            val (idx, subres) = pf(msg)
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

}
