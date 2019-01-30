/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala

import scala.language.experimental.macros
import scala.concurrent.{Future, ExecutionContext}
import scala.annotation.compileTimeOnly

/**
 * Async blocks provide a direct means to work with [[scala.concurrent.Future]].
 *
 * For example, to use an API that fetches a web page to fetch
 * two pages and add their lengths:
 *
 * {{{
 *  import ExecutionContext.Implicits.global
 *  import scala.async.{async, await}
 *
 *  def fetchURL(url: URL): Future[String] = ...
 *
 *  val sumLengths: Future[Int] = async {
 *    val body1 = fetchURL("http://scala-lang.org")
 *    val body2 = fetchURL("http://docs.scala-lang.org")
 *    await(body1).length + await(body2).length
 *  }
 * }}}
 *
 * Note that in the following program, the second fetch does *not* start
 * until after the first. If you need to start tasks in parallel, you must do
 * so before `await`-ing a result.
 *
 * {{{
 *  val sumLengths: Future[Int] = async {
 *    await(fetchURL("http://scala-lang.org")).length + await(fetchURL("http://docs.scala-lang.org")).length
 *  }
 * }}}
 */
object async {
  /**
   * Run the block of code `body` asynchronously. `body` may contain calls to `await` when the results of
   * a `Future` are needed; this is translated into non-blocking code.
   */
  def async[T](body: T)(implicit execContext: ExecutionContext): Future[T] = macro ???

  /**
   * Non-blocking await the on result of `awaitable`. This may only be used directly within an enclosing `async` block.
   *
   * Internally, this will register the remainder of the code in enclosing `async` block as a callback
   * in the `onComplete` handler of `awaitable`, and will *not* block a thread.
   */
  @compileTimeOnly("`await` must be enclosed in an `async` block")
  def await[T](awaitable: Future[T]): T = ??? // No implementation here, as calls to this are translated to `onComplete` by the macro.
}
