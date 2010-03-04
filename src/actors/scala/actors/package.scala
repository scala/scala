package scala

package object actors {

  @deprecated("use scala.actors.scheduler.ForkJoinScheduler instead")
  type FJTaskScheduler2 = scala.actors.scheduler.ForkJoinScheduler

  @deprecated("use scala.actors.scheduler.ForkJoinScheduler instead")
  type TickedScheduler = scala.actors.scheduler.ForkJoinScheduler

  @deprecated("use scala.actors.scheduler.ForkJoinScheduler instead")
  type WorkerThreadScheduler = scala.actors.scheduler.ForkJoinScheduler

  @deprecated("this class is going to be removed in a future release")
  type WorkerThread = java.lang.Thread

  @deprecated("use scala.actors.scheduler.SingleThreadedScheduler instead")
  type SingleThreadedScheduler = scala.actors.scheduler.SingleThreadedScheduler

  @deprecated("this value is going to be removed in a future release")
  val ActorGC = scala.actors.Scheduler.impl.asInstanceOf[scala.actors.scheduler.ActorGC]

}
