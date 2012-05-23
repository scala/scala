/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.actors

import scala.actors.scheduler.{DelegatingScheduler, ExecutorScheduler,
                               ForkJoinScheduler, ThreadPoolConfig}
import java.util.concurrent.{ThreadPoolExecutor, TimeUnit, LinkedBlockingQueue}

private[actors] object Reactor {

  val scheduler = new DelegatingScheduler {
    def makeNewScheduler: IScheduler = {
      val sched = if (!ThreadPoolConfig.useForkJoin) {
        // default is non-daemon
        val workQueue = new LinkedBlockingQueue[Runnable]
        ExecutorScheduler(
          new ThreadPoolExecutor(ThreadPoolConfig.corePoolSize,
                                 ThreadPoolConfig.maxPoolSize,
                                 60000L,
                                 TimeUnit.MILLISECONDS,
                                 workQueue,
                                 new ThreadPoolExecutor.CallerRunsPolicy))
      } else {
        // default is non-daemon, non-fair
        val s = new ForkJoinScheduler(ThreadPoolConfig.corePoolSize, ThreadPoolConfig.maxPoolSize, false, false)
        s.start()
        s
      }
      Debug.info(this+": starting new "+sched+" ["+sched.getClass+"]")
      sched
    }
  }

  val waitingForNone: PartialFunction[Any, Unit] = new PartialFunction[Any, Unit] {
    def isDefinedAt(x: Any) = false
    def apply(x: Any) {}
  }
}

/**
 * Super trait of all actor traits.
 *
 * @author Philipp Haller
 *
 * @define actor reactor
 */
trait Reactor[Msg >: Null] extends OutputChannel[Msg] with Combinators {

  /* The $actor's mailbox. */
  private[actors] val mailbox = new MQueue[Msg]("Reactor")

  // guarded by this
  private[actors] val sendBuffer = new MQueue[Msg]("SendBuffer")

  /* Whenever this $actor executes on some thread, `waitingFor` is
   * guaranteed to be equal to `Reactor.waitingForNone`.
   *
   * In other words, whenever `waitingFor` is not equal to
   * `Reactor.waitingForNone`, this $actor is guaranteed not to execute
   * on some thread.
   *
   * If the $actor waits in a `react`, `waitingFor` holds the
   * message handler that `react` was called with.
   *
   * guarded by this
   */
  private[actors] var waitingFor: PartialFunction[Msg, Any] =
    Reactor.waitingForNone

  // guarded by this
  private[actors] var _state: Actor.State.Value = Actor.State.New

  /**
   * The $actor's behavior is specified by implementing this method.
   */
  def act(): Unit

  /**
   * This partial function is applied to exceptions that propagate out of
   * this $actor's body.
   */
  protected[actors] def exceptionHandler: PartialFunction[Exception, Unit] =
    Map()

  protected[actors] def scheduler: IScheduler =
    Reactor.scheduler

  protected[actors] def mailboxSize: Int =
    mailbox.size

  def send(msg: Msg, replyTo: OutputChannel[Any]) {
    val todo = synchronized {
      if (waitingFor ne Reactor.waitingForNone) {
        val savedWaitingFor = waitingFor
        waitingFor = Reactor.waitingForNone
        startSearch(msg, replyTo, savedWaitingFor)
      } else {
        sendBuffer.append(msg, replyTo)
        () => { /* do nothing */ }
      }
    }
    todo()
  }

  private[actors] def startSearch(msg: Msg, replyTo: OutputChannel[Any], handler: PartialFunction[Msg, Any]) =
    () => scheduler execute makeReaction(() => {
      val startMbox = new MQueue[Msg]("Start")
      synchronized { startMbox.append(msg, replyTo) }
      searchMailbox(startMbox, handler, true)
    })

  private[actors] final def makeReaction(fun: () => Unit): Runnable =
    makeReaction(fun, null, null)

  /* This method is supposed to be overridden. */
  private[actors] def makeReaction(fun: () => Unit, handler: PartialFunction[Msg, Any], msg: Msg): Runnable =
    new ReactorTask(this, fun, handler, msg)

  private[actors] def resumeReceiver(item: (Msg, OutputChannel[Any]), handler: PartialFunction[Msg, Any], onSameThread: Boolean) {
    if (onSameThread)
      makeReaction(null, handler, item._1).run()
    else
      scheduleActor(handler, item._1)

    /* Here, we throw a SuspendActorControl to avoid
       terminating this actor when the current ReactorTask
       is finished.

       The SuspendActorControl skips the termination code
       in ReactorTask.
     */
    throw Actor.suspendException
  }

  def !(msg: Msg) {
    send(msg, null)
  }

  def forward(msg: Msg) {
    send(msg, null)
  }

  def receiver: Actor = this.asInstanceOf[Actor]

  // guarded by this
  private[actors] def drainSendBuffer(mbox: MQueue[Msg]) {
    sendBuffer.foreachDequeue(mbox)
  }

  private[actors] def searchMailbox(startMbox: MQueue[Msg],
                                    handler: PartialFunction[Msg, Any],
                                    resumeOnSameThread: Boolean) {
    var tmpMbox = startMbox
    var done = false
    while (!done) {
      val qel = tmpMbox.extractFirst(handler)
      if (tmpMbox ne mailbox)
        tmpMbox.foreachAppend(mailbox)
      if (null eq qel) {
        synchronized {
          // in mean time new stuff might have arrived
          if (!sendBuffer.isEmpty) {
            tmpMbox = new MQueue[Msg]("Temp")
            drainSendBuffer(tmpMbox)
            // keep going
          } else {
            waitingFor = handler
            /* Here, we throw a SuspendActorControl to avoid
               terminating this actor when the current ReactorTask
               is finished.

               The SuspendActorControl skips the termination code
               in ReactorTask.
             */
            throw Actor.suspendException
          }
        }
      } else {
        resumeReceiver((qel.msg, qel.session), handler, resumeOnSameThread)
        done = true
      }
    }
  }

  /**
   * Receives a message from this $actor's mailbox.
   *
   * This method never returns. Therefore, the rest of the computation
   * has to be contained in the actions of the partial function.
   *
   * @param  handler  a partial function with message patterns and actions
   */
  protected def react(handler: PartialFunction[Msg, Unit]): Nothing = {
    synchronized { drainSendBuffer(mailbox) }
    searchMailbox(mailbox, handler, false)
    throw Actor.suspendException
  }

  /* This method is guaranteed to be executed from inside
   * an $actor's act method.
   *
   * assume handler != null
   *
   * never throws SuspendActorControl
   */
  private[actors] def scheduleActor(handler: PartialFunction[Msg, Any], msg: Msg) {
    scheduler executeFromActor makeReaction(null, handler, msg)
  }

  private[actors] def preAct() = {}

  // guarded by this
  private[actors] def dostart() {
    _state = Actor.State.Runnable
    scheduler newActor this
    scheduler execute makeReaction(() => {
      preAct()
      act()
    }, null, null)
  }

  /**
   * Starts this $actor. This method is idempotent.
   */
  def start(): Reactor[Msg] = synchronized {
    if (_state == Actor.State.New)
      dostart()
    this
  }

  /**
   * Restarts this $actor.
   *
   * @throws java.lang.IllegalStateException  if the $actor is not in state `Actor.State.Terminated`
   */
  def restart(): Unit = synchronized {
    if (_state == Actor.State.Terminated)
      dostart()
    else
      throw new IllegalStateException("restart only in state "+Actor.State.Terminated)
  }

  /** Returns the execution state of this $actor.
   *
   *  @return the execution state
   */
  def getState: Actor.State.Value = synchronized {
    if (waitingFor ne Reactor.waitingForNone)
      Actor.State.Suspended
    else
      _state
  }

  implicit def mkBody[A](body: => A) = new InternalActor.Body[A] {
    def andThen[B](other: => B): Unit = Reactor.this.seq(body, other)
  }

  /* This closure is used to implement control-flow operations
   * built on top of `seq`. Note that the only invocation of
   * `kill` is supposed to be inside `ReactorTask.run`.
   */
  @volatile
  private[actors] var kill: () => Unit =
    () => { exit() }

  private[actors] def seq[a, b](first: => a, next: => b): Unit = {
    val killNext = this.kill
    this.kill = () => {
      this.kill = killNext

      // to avoid stack overflow:
      // instead of directly executing `next`,
      // schedule as continuation
      scheduleActor({ case _ => next }, null)
      throw Actor.suspendException
    }
    first
    throw new KillActorControl
  }

  protected[actors] def exit(): Nothing = {
    terminated()
    throw Actor.suspendException
  }

  private[actors] def internalPostStop() = {}

  private[actors] def terminated() {
    synchronized {
      _state = Actor.State.Terminated
      // reset waitingFor, otherwise getState returns Suspended
      waitingFor = Reactor.waitingForNone
    }
    internalPostStop()
    scheduler.terminated(this)
  }

}
