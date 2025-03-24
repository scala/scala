/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.concurrent

/**
 * A context to be notified by [[scala.concurrent.blocking]] when
 * a thread is about to block. In effect this trait provides
 * the implementation for [[scala.concurrent.Await]].
 * [[scala.concurrent.Await.result]] and [[scala.concurrent.Await.ready]]
 * locates an instance of `BlockContext` by first looking for one
 * provided through [[BlockContext.withBlockContext]] and failing that,
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
 *    override def blockOn[T](thunk: => T)(implicit permission: CanAwait): T = {
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
    * Designates (and eventually executes) a thunk which potentially blocks the calling `java.lang.Thread`.
    *
    * Clients must use `scala.concurrent.blocking` or `scala.concurrent.Await` instead.
    *
    * In implementations of this method it is RECOMMENDED to first check if `permission` is `null` and
    * if it is, throw an `IllegalArgumentException`.
    *
    * @throws IllegalArgumentException if the `permission` is `null`
    */
  def blockOn[T](thunk: => T)(implicit permission: CanAwait): T
}

object BlockContext {
  private[this] object DefaultBlockContext extends BlockContext {
    override final def blockOn[T](thunk: => T)(implicit permission: CanAwait): T = thunk
  }

  /**
    * The default block context will execute the supplied thunk immediately.
    * @return the `BlockContext` that will be used if no other is found.
    **/
  final def defaultBlockContext: BlockContext = DefaultBlockContext

  private[this] final val contextLocal = new ThreadLocal[BlockContext]()

  private[this] final def prefer(candidate: BlockContext): BlockContext =
    if (candidate ne null) candidate
    else {
      val t = Thread.currentThread
      if (t.isInstanceOf[BlockContext]) t.asInstanceOf[BlockContext]
      else DefaultBlockContext
    }

  /**
   * @return the `BlockContext` that would be used for the current `java.lang.Thread` at this point
   **/
  final def current: BlockContext = prefer(contextLocal.get)

  /**
   * Installs a current `BlockContext` around executing `body`.
   **/
  final def withBlockContext[T](blockContext: BlockContext)(body: => T): T = {
    val old = contextLocal.get // can be null
    if (old eq blockContext) body
    else {
      contextLocal.set(blockContext)
      try body finally contextLocal.set(old)
    }
  }

  /**
   * Installs the BlockContext `blockContext` around the invocation to `f` and passes in the previously installed BlockContext to `f`.
   * @return the value produced by applying `f`
   **/
  final def usingBlockContext[I, T](blockContext: BlockContext)(f: BlockContext => T): T = {
    val old = contextLocal.get // can be null
    if (old eq blockContext) f(prefer(old))
    else {
      contextLocal.set(blockContext)
      try f(prefer(old)) finally contextLocal.set(old)
    }
  }
}
