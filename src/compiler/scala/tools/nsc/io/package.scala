/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc

import java.util.concurrent.{ Future, Callable }
import java.util.{ Timer, TimerTask }
import java.util.jar.{ Attributes }

package object io {
  type JManifest = java.util.jar.Manifest
  type JFile = java.io.File

  implicit def enrichManifest(m: JManifest): Jar.WManifest = Jar.WManifest(m)
  private lazy val daemonThreadPool = DaemonThreadFactory.newPool()

  def runnable(body: => Unit): Runnable       = new Runnable { override def run() = body }
  def callable[T](body: => T): Callable[T]    = new Callable[T] { override def call() = body }
  def spawn[T](body: => T): Future[T]         = daemonThreadPool submit callable(body)
  def submit(runnable: Runnable)              = daemonThreadPool submit runnable
  def runnableFn(f: () => Unit): Runnable     = runnable(f())
  def callableFn[T](f: () => T): Callable[T]  = callable(f())
  def spawnFn[T](f: () => T): Future[T]       = spawn(f())

  // Create, start, and return a daemon thread
  def daemonize(body: => Unit): Thread = newThread(_ setDaemon true)(body)
  def newThread(f: Thread => Unit)(body: => Unit): Thread = {
    val thread = new Thread(runnable(body))
    f(thread)
    thread.start
    thread
  }

  // Set a timer to execute the given code.
  def timer(seconds: Int)(body: => Unit): Timer = {
    val alarm = new Timer(true) // daemon
    val tt    = new TimerTask { def run() = body }

    alarm.schedule(tt, seconds * 1000)
    alarm
  }
}