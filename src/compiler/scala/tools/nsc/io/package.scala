/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc

import java.util.concurrent.{ Future, Callable, Executors }

package object io {
  def runnable(body: => Unit): Runnable       = new Runnable { override def run() = body }
  def callable[T](body: => T): Callable[T]    = new Callable[T] { override def call() = body }
  def spawn[T](body: => T): Future[T]         = Executors.newSingleThreadExecutor() submit callable[T](body)
  def submit(runnable: Runnable)              = Executors.newSingleThreadExecutor() submit runnable
  def runnableFn(f: () => Unit): Runnable     = runnable(f())
  def callableFn[T](f: () => T): Callable[T]  = callable(f())
  def spawnFn[T](f: () => T): Future[T]       = spawn(f())
}