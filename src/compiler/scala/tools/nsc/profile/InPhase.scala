package scala.tools.nsc.profile

import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.tools.nsc.{Global, Phase}
object InPhase {
  val idGen = new AtomicInteger
}
/**
  * A wrapper to allow actions to be associated to a Phase. This aids profiling, particularly where a actions occur in
  * multiple threads, or out of order
  *
  * When you are running a compilation task that involved some activity on a background thread
  * (not the one running [[Global.compileUnits]]) the profiler is not aware of that thread and so cannot account
  * for the activity.
  *
  * By wrapping the activity in this class or one of it children the profiler (if enabled) is informed
  * and the statistics can be gathered
  *
  * No InPhase should run concurrently with another InPhase on the same thread - the statistics dont cope with nesting
  */
sealed abstract class InPhase(global: Global, val phase:Phase, val comment:String) {

  private[profile] final val id = InPhase.idGen.incrementAndGet()
  private[profile] final val profiler = global.currentRun.profiler
  private[profile] final var idleNs = 0L
  profiler.registerInPhase(this)

  @inline protected [profile] def doAction[T] (fn : => T) : T = {
    val before = profiler.beforeInPhase(this)
    try fn
    finally profiler.afterInPhase(this, before, idleNs)
  }

  /**
    * If the compilation activity has some idle time waiting on a future, then this can be recorded by
    * using this method to perform the wait for you. This allow the profiler to distinguish idle time (waiting for some
    * related activity to complete), from for example waiting on I/O
    * @param future the future that you are waiting on
    * @param duration the maximum duration to wait
    */
  def idle(future: Future[_], duration:Duration = Duration.Inf): Unit = {
    if (!future.isCompleted) {
      val start = System.nanoTime()
      try Await.ready(future, duration)
      finally idleNs += (System.nanoTime() - start)
    }
  }

}
/**
  * an InPhase for Runnables
  *
  * By enclosing the activity in the doRun method of this class the profiler (if enabled) is informed
  * and the statistics can be gathered
  */

object RunnableInPhase {
  def apply(global: Global, phase:Phase, comment:String)(fn: => Unit)(implicit executionContext: ExecutionContext) = {
    new RunnableInPhase(global, phase, comment)(fn)
  }
}
class RunnableInPhase(global: Global, phase:Phase, comment:String)(fn: => Unit) extends InPhase(global, phase, comment) with Runnable {
  final def run(): Unit = doAction(fn)
}

/**
  * an InPhase for Futures
  *
  * By enclosing the activity in this wrapper the profiler (if enabled) is informed
  * and the statistics can be gathered
  */
object FutureInPhase {
  def apply[T](global: Global, phase:Phase, comment:String)(fn: => T)(implicit executionContext: ExecutionContext) = {
    val inPhase = new FutureInPhase(global, phase, comment)(fn)
    Future(inPhase.exec())
  }
}

class FutureInPhase[T](global: Global, phase:Phase, comment:String)(fn: => T) extends InPhase(global, phase, comment) {
  final def exec() = doAction(fn)
}