/*
 * Copyright 2007 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Rework of David Pollak's ActorPing class in the Lift Project
 * which is licensed under the Apache 2 License.
 */
package akka.actor

import scala.collection.JavaConversions

import java.util.concurrent._

import akka.event.EventHandler
import akka.AkkaException

object Scheduler {
  import Actor._

  case class SchedulerException(msg: String, e: Throwable) extends RuntimeException(msg, e)

  @volatile
  private var service = Executors.newSingleThreadScheduledExecutor(SchedulerThreadFactory)

  /**
   * Schedules to send the specified message to the receiver after initialDelay and then repeated after delay
   */
  def schedule(receiver: ActorRef, message: AnyRef, initialDelay: Long, delay: Long, timeUnit: TimeUnit): ScheduledFuture[AnyRef] = {
    try {
      service.scheduleAtFixedRate(
        new Runnable { def run = receiver ! message },
        initialDelay, delay, timeUnit).asInstanceOf[ScheduledFuture[AnyRef]]
    } catch {
      case e: Exception =>
        val error = SchedulerException(message + " could not be scheduled on " + receiver, e)
        EventHandler.error(error, this, "%s @ %s".format(receiver, message))
        throw error
    }
  }

  /**
   * Schedules to run specified function to the receiver after initialDelay and then repeated after delay,
   * avoid blocking operations since this is executed in the schedulers thread
   */
  def schedule(f: () => Unit, initialDelay: Long, delay: Long, timeUnit: TimeUnit): ScheduledFuture[AnyRef] =
    schedule(new Runnable { def run = f() }, initialDelay, delay, timeUnit)

  /**
   * Schedules to run specified runnable to the receiver after initialDelay and then repeated after delay,
   * avoid blocking operations since this is executed in the schedulers thread
   */
  def schedule(runnable: Runnable, initialDelay: Long, delay: Long, timeUnit: TimeUnit): ScheduledFuture[AnyRef] = {
    try {
      service.scheduleAtFixedRate(runnable, initialDelay, delay, timeUnit).asInstanceOf[ScheduledFuture[AnyRef]]
    } catch {
      case e: Exception =>
        val error = SchedulerException("Failed to schedule a Runnable", e)
        EventHandler.error(error, this, error.getMessage)
        throw error
    }
  }

  /**
   * Schedules to send the specified message to the receiver after delay
   */
  def scheduleOnce(receiver: ActorRef, message: AnyRef, delay: Long, timeUnit: TimeUnit): ScheduledFuture[AnyRef] = {
    try {
      service.schedule(
        new Runnable { def run = receiver ! message },
        delay, timeUnit).asInstanceOf[ScheduledFuture[AnyRef]]
    } catch {
      case e: Exception =>
        val error = SchedulerException(message + " could not be scheduleOnce'd on " + receiver, e)
        EventHandler.error(e, this, receiver + " @ " + message)
        throw error
    }
  }

  /**
   * Schedules a function to be run after delay,
   * avoid blocking operations since the runnable is executed in the schedulers thread
   */
  def scheduleOnce(f: () => Unit, delay: Long, timeUnit: TimeUnit): ScheduledFuture[AnyRef] =
    scheduleOnce(new Runnable { def run = f() }, delay, timeUnit)

  /**
   * Schedules a runnable to be run after delay,
   * avoid blocking operations since the runnable is executed in the schedulers thread
   */
  def scheduleOnce(runnable: Runnable, delay: Long, timeUnit: TimeUnit): ScheduledFuture[AnyRef] = {
    try {
      service.schedule(runnable, delay, timeUnit).asInstanceOf[ScheduledFuture[AnyRef]]
    } catch {
      case e: Exception =>
        val error = SchedulerException("Failed to scheduleOnce a Runnable", e)
        EventHandler.error(e, this, error.getMessage)
        throw error
    }
  }

  def shutdown() {
    synchronized {
      service.shutdown()
    }
  }

  def restart() {
    synchronized {
      shutdown()
      service = Executors.newSingleThreadScheduledExecutor(SchedulerThreadFactory)
    }
  }
}

private object SchedulerThreadFactory extends ThreadFactory {
  private var count = 0
  val threadFactory = Executors.defaultThreadFactory()

  def newThread(r: Runnable): Thread = {
    val thread = threadFactory.newThread(r)
    thread.setName("akka:scheduler-" + count)
    thread.setDaemon(true)
    thread
  }
}
