/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.actors

import scala.util.control.ControlThrowable
import java.util.{Timer, TimerTask}
import language.implicitConversions

/**
 * Provides functions for the definition of actors, as well as actor
 * operations, such as `receive`, `react`, `reply`, etc.
 *
 * @author Philipp Haller
 */
object Actor extends Combinators {

  /** State of an actor.
   *
   *  - '''New''' -
   *      Not yet started
   *  - '''Runnable''' -
   *      Executing
   *  - '''Suspended''' -
   *      Suspended, waiting in a `react`
   *  - '''TimedSuspended''' -
   *      Suspended, waiting in a `reactWithin`
   *  - '''Blocked''' -
   *      Blocked waiting in a `receive`
   *  - '''TimedBlocked''' -
   *      Blocked waiting in a `receiveWithin`
   *  - '''Terminated''' -
   *      Actor has terminated
   */
  object State extends Enumeration {
    val New,
        Runnable,
        Suspended,
        TimedSuspended,
        Blocked,
        TimedBlocked,
        Terminated = Value
  }

  private[actors] val tl = new ThreadLocal[InternalReplyReactor]

  // timer thread runs as daemon
  private[actors] val timer = new Timer(true)

  private[actors] val suspendException = new SuspendActorControl

  /**
   * Returns the currently executing actor. Should be used instead
   * of `'''this'''` in all blocks of code executed by actors.
   *
   * @return returns the currently executing actor.
   */
  def self: Actor = self(Scheduler).asInstanceOf[Actor]

  private[actors] def self(sched: IScheduler): InternalActor =
    rawSelf(sched).asInstanceOf[InternalActor]

  private[actors] def rawSelf: InternalReplyReactor =
    rawSelf(Scheduler)

  private[actors] def rawSelf(sched: IScheduler): InternalReplyReactor = {
    val s = tl.get
    if (s eq null) {
      val r = new ActorProxy(Thread.currentThread, sched)
      tl.set(r)
      r
    } else
      s
  }

  private def parentScheduler: IScheduler = {
    val s = tl.get
    if (s eq null) Scheduler else s.scheduler
  }

  /**
   * Resets an actor proxy associated with the current thread.
   * It replaces the implicit `ActorProxy` instance
   * of the current thread (if any) with a new instance.
   *
   * This permits to re-use the current thread as an actor
   * even if its `ActorProxy` has died for some reason.
   */
  def resetProxy() {
    val a = tl.get
    if ((null ne a) && a.isInstanceOf[ActorProxy])
      tl.set(new ActorProxy(Thread.currentThread, parentScheduler))
  }

  /**
   * Removes any reference to an `Actor` instance
   * currently stored in thread-local storage.
   *
   * This allows to release references from threads that are potentially
   * long-running or being re-used (e.g. inside a thread pool). Permanent
   * references in thread-local storage are a potential memory leak.
   */
  def clearSelf() {
    tl set null
  }

  /**
   * Factory method for creating and starting an actor.
   *
   * @example {{{
   * import scala.actors.Actor._
   * ...
   * val a = actor {
   *   ...
   * }
   * }}}
   *
   * @param  body  the code block to be executed by the newly created actor
   * @return       the newly created actor. Note that it is automatically started.
   */
  def actor(body: => Unit): Actor = {
    val a = new Actor {
      def act() = body
      override final val scheduler: IScheduler = parentScheduler
    }
    a.start()
    a
  }

  /**
   * Factory method for creating actors whose
   * body is defined using a `Responder`.
   *
   * @example {{{
   * import scala.actors.Actor._
   * import Responder.exec
   * ...
   * val a = reactor {
   *   for {
   *     res <- b !! MyRequest;
   *     if exec(println("result: "+res))
   *   } yield {}
   * }
   * }}}
   *
   * @param  body  the `Responder` to be executed by the newly created actor
   * @return       the newly created actor. Note that it is automatically started.
   */
  def reactor(body: => Responder[Unit]): Actor = {
    val a = new Actor {
      def act() {
        Responder.run(body)
      }
      override final val scheduler: IScheduler = parentScheduler
    }
    a.start()
    a
  }

  /**
   * Receives the next message from the mailbox of the current actor `self`.
   */
  def ? : Any = self.?

  /**
   * Receives a message from the mailbox of `self`. Blocks if no message
   * matching any of the cases of `f` can be received.
   *
   * @example {{{
   * receive {
   *   case "exit" => println("exiting")
   *   case 42 => println("got the answer")
   *   case x:Int => println("got an answer")
   * }
   * }}}
   *
   * @param  f a partial function specifying patterns and actions
   * @return   the result of processing the received message
   */
  def receive[A](f: PartialFunction[Any, A]): A =
    self.receive(f)

  /**
   * Receives a message from the mailbox of `self`. Blocks at most `msec`
   * milliseconds if no message matching any of the cases of `f` can be
   * received. If no message could be received the `TIMEOUT` action is
   * executed if specified.
   *
   * @param  msec the time span before timeout
   * @param  f    a partial function specifying patterns and actions
   * @return      the result of processing the received message
   */
  def receiveWithin[R](msec: Long)(f: PartialFunction[Any, R]): R =
    self.receiveWithin(msec)(f)

  /**
   * Lightweight variant of `receive`.
   *
   * Actions in `f` have to contain the rest of the computation of `self`,
   * as this method will never return.
   *
   * A common method of continuting the computation is to send a message
   * to another actor:
   * {{{
   * react {
   *   case Get(from) =>
   *     react {
   *       case Put(x) => from ! x
   *     }
   * }
   * }}}
   *
   * Another common method is to use `loop` to continuously `react` to messages:
   * {{{
   * loop {
   *   react {
   *     case Msg(data) => // process data
   *   }
   * }
   * }}}
   *
   * @param  f a partial function specifying patterns and actions
   * @return   this function never returns
   */
  def react(f: PartialFunction[Any, Unit]): Nothing =
    rawSelf.react(f)

  /**
   * Lightweight variant of `receiveWithin`.
   *
   * Actions in `f` have to contain the rest of the computation of `self`,
   * as this method will never return.
   *
   * @param  msec the time span before timeout
   * @param  f    a partial function specifying patterns and actions
   * @return      this function never returns
   */
  def reactWithin(msec: Long)(f: PartialFunction[Any, Unit]): Nothing =
    self.reactWithin(msec)(f)

  def eventloop(f: PartialFunction[Any, Unit]): Nothing =
    rawSelf.react(new RecursiveProxyHandler(rawSelf, f))

  private class RecursiveProxyHandler(a: InternalReplyReactor, f: PartialFunction[Any, Unit])
          extends PartialFunction[Any, Unit] {
    def isDefinedAt(m: Any): Boolean =
      true // events are immediately removed from the mailbox
    def apply(m: Any) {
      if (f.isDefinedAt(m)) f(m)
      a.react(this)
    }
  }

  /**
   * Returns the actor which sent the last received message.
   */
  def sender: OutputChannel[Any] =
    rawSelf.internalSender

  /**
   * Sends `msg` to the actor waiting in a call to `!?`.
   */
  def reply(msg: Any): Unit =
    rawSelf.reply(msg)

  /**
   * Sends `()` to the actor waiting in a call to `!?`.
   */
  def reply(): Unit =
    rawSelf.reply(())

  /**
   * Returns the number of messages in `self`'s mailbox
   *
   * @return the number of messages in `self`'s mailbox
   */
  def mailboxSize: Int = rawSelf.mailboxSize

  /**
   * Converts a synchronous event-based operation into
   * an asynchronous `Responder`.
   *
   * @example {{{
   * val adder = reactor {
   *   for {
   *     _ <- respondOn(react) { case Add(a, b) => reply(a+b) }
   *   } yield {}
   * }
   * }}}
   */
  def respondOn[A, B](fun: PartialFunction[A, Unit] => Nothing):
    PartialFunction[A, B] => Responder[B] =
      (caseBlock: PartialFunction[A, B]) => new Responder[B] {
        def respond(k: B => Unit) = fun(caseBlock andThen k)
      }

  private[actors] trait Body[a] {
    def andThen[b](other: => b): Unit
  }

  implicit def mkBody[a](body: => a) = new InternalActor.Body[a] {
    def andThen[b](other: => b): Unit = rawSelf.seq(body, other)
  }

  /**
   * Links `self` to actor `to`.
   *
   * @param  to the actor to link to
   * @return    the parameter actor
   */
  def link(to: AbstractActor): AbstractActor = self.link(to)

  /**
   * Links `self` to the actor defined by `body`.
   *
   * @param body the body of the actor to link to
   * @return     the parameter actor
   */
  def link(body: => Unit): Actor = self.link(body)

  /**
   * Unlinks `self` from actor `from`.
   *
   * @param from the actor to unlink from
   */
  def unlink(from: AbstractActor): Unit = self.unlink(from)

  /**
   * Terminates execution of `self` with the following effect on
   * linked actors:
   *
   * For each linked actor `a` with `trapExit` set to `'''true'''`,
   * send message `Exit(self, reason)` to `a`.
   *
   * For each linked actor `a` with `trapExit` set to `'''false'''`
   * (default), call `a.exit(reason)` if `reason != 'normal`.
   */
  def exit(reason: AnyRef): Nothing = self.exit(reason)

  /**
   * Terminates execution of `self` with the following effect on
   * linked actors:
   *
   * For each linked actor `a` with `trapExit` set to `'''true'''`,
   * send message `Exit(self, 'normal)` to `a`.
   */
  def exit(): Nothing = rawSelf.exit()

}

/** Provides lightweight, concurrent actors. Actors are created by extending
 *  the `Actor` trait (alternatively, one of the factory methods in its
 *  companion object can be used).  The behavior of an `Actor` subclass is
 *  defined by implementing its `act` method:
 *  {{{
 *  class MyActor extends Actor {
 *    def act() {
 *      // actor behavior goes here
 *    }
 *  }
 *  }}}
 *  A new `Actor` instance is started by invoking its `start` method.
 *
 *  '''Note:''' care must be taken when invoking thread-blocking methods other
 *  than those provided by the `Actor` trait or its companion object (such as
 *  `receive`). Blocking the underlying thread inside an actor may lead to
 *  starvation of other actors. This also applies to actors hogging their
 *  thread for a long time between invoking `receive`/`react`.
 *
 *  If actors use blocking operations (for example, methods for blocking I/O),
 *  there are several options:
 *
 *  - The run-time system can be configured to use a larger thread pool size
 *    (for example, by setting the `actors.corePoolSize` JVM property).
 *  - The `scheduler` method of the `Actor` trait can be overridden to return a
 *    `ResizableThreadPoolScheduler`, which resizes its thread pool to
 *    avoid starvation caused by actors that invoke arbitrary blocking methods.
 *  - The `actors.enableForkJoin` JVM property can be set to `false`, in which
 *    case a `ResizableThreadPoolScheduler` is used by default to execute actors.
 *
 *  The main ideas of the implementation are explained in the two papers
 *
 *  - [[http://lampwww.epfl.ch/~odersky/papers/jmlc06.pdf Event-Based
 *    Programming without Inversion of Control]],
 *    Philipp Haller and Martin Odersky, ''Proc. JMLC 2006'', and
 *  - [[http://lamp.epfl.ch/~phaller/doc/haller07coord.pdf Actors that
 *    Unify Threads and Events]],
 *    Philipp Haller and Martin Odersky, ''Proc. COORDINATION 2007''.
 *
 *  @author Philipp Haller
 *
 *  @define actor actor
 *  @define channel actor's mailbox
 */
@SerialVersionUID(-781154067877019505L)
trait Actor extends InternalActor with ReplyReactor {

  override def start(): Actor = synchronized {
    super.start()
    this
  }

  }

