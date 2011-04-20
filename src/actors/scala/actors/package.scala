package scala

/**
 * A library that provides both asynchronous and synchronous messaging to allow
 * for concurrent programming without explicit synchronization.
 *
 * == Guide ==
 *
 * A detailed guide for the actors library is available
 * [[http://www.scala-lang.org/docu/files/actors-api/actors_api_guide.html#]].
 *
 * == Getting Started ==
 *
 * A starting point for using the actors library would be [[scala.actors.Reactor]],
 * [[scala.actors.ReplyReactor]], or [[scala.actors.Actor]] or their companion objects.
 *
 */
package object actors {

  // type of Reactors tracked by termination detector
  private[actors] type TrackedReactor = Reactor[A] forSome { type A >: Null }

  @deprecated("use scheduler.ForkJoinScheduler instead", "2.8.0")
  type FJTaskScheduler2 = scala.actors.scheduler.ForkJoinScheduler

  @deprecated("use scheduler.ForkJoinScheduler instead", "2.8.0")
  type TickedScheduler = scala.actors.scheduler.ForkJoinScheduler

  @deprecated("use scheduler.ForkJoinScheduler instead", "2.8.0")
  type WorkerThreadScheduler = scala.actors.scheduler.ForkJoinScheduler

  @deprecated("this class is going to be removed in a future release", "2.8.0")
  type WorkerThread = java.lang.Thread

  @deprecated("use scheduler.SingleThreadedScheduler instead", "2.8.0")
  type SingleThreadedScheduler = scala.actors.scheduler.SingleThreadedScheduler

  // This used to do a blind cast and throw a CCE after the package
  // object was loaded.  I have replaced with a variation that should work
  // in whatever cases that was working but fail less exceptionally for
  // those not intentionally using it.
  @deprecated("this value is going to be removed in a future release", "2.8.0")
  val ActorGC = scala.actors.Scheduler.impl match {
    case x: scala.actors.scheduler.ActorGC  => x
    case _                                  => null
  }
}
