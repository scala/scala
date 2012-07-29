/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2012, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent

import java.lang.Thread
import scala.concurrent.util.Duration

/**
 * A context to be notified by `scala.concurrent.blocking` when
 * a thread is about to block. In effect this trait provides
 * the implementation for `scala.concurrent.Await`.
 * `scala.concurrent.Await.result()` and `scala.concurrent.Await.ready()`
 * locates an instance of `BlockContext` by first looking for one
 * provided through `BlockContext.withBlockContext()` and failing that,
 * checking whether `Thread.currentThread` is an instance of `BlockContext`.
 * So a thread pool can have its `java.lang.Thread` instances implement
 * `BlockContext`. There's a default `BlockContext` used if the thread
 * doesn't implement `BlockContext`.
 *
 * Typically, you'll want to chain to the previous `BlockContext`,
 * like this:
 * {{{
 *  val oldContext = BlockContext.current
 *  val myContext = new BlockContext {
 *    override def blockOn[T](thunk: =>T)(implicit permission: CanAwait): T = {
 *      // you'd have code here doing whatever you need to do
 *      // when the thread is about to block.
 *      // Then you'd chain to the previous context:
 *      oldContext.blockOn(thunk)
 *    }
 *  }
 *  BlockContext.withBlockContext(myContext) {
 *    // then this block runs with myContext as the handler
 *    // for scala.concurrent.blocking
 *  }
 *  }}}
 */
trait BlockContext {

  /** Used internally by the framework;
   * Designates (and eventually executes) a thunk which potentially blocks the calling `Thread`.
   *
   * Clients must use `scala.concurrent.blocking` or `scala.concurrent.Await` instead.
   */
  def blockOn[T](thunk: =>T)(implicit permission: CanAwait): T
}

object BlockContext {
  private object DefaultBlockContext extends BlockContext {
    override def blockOn[T](thunk: =>T)(implicit permission: CanAwait): T = thunk
  }

  private val contextLocal = new ThreadLocal[BlockContext]()

  /** Obtain the current thread's current `BlockContext`. */
  def current: BlockContext = contextLocal.get match {
    case null => Thread.currentThread match {
      case ctx: BlockContext => ctx
      case _ => DefaultBlockContext
    }
    case some => some
  }

  /** Pushes a current `BlockContext` while executing `body`. */
  def withBlockContext[T](blockContext: BlockContext)(body: => T): T = {
    val old = contextLocal.get // can be null
    try {
      contextLocal.set(blockContext)
      body
    } finally {
      contextLocal.set(old)
    }
  }
}
