/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent.impl
import scala.concurrent.{ExecutionContext, CanAwait, TimeoutException, ExecutionException, Future, Batchable }
import Future.InternalCallbackExecutor
import scala.concurrent.duration.Duration
import scala.annotation.{ tailrec, switch }
import scala.util.control.{ NonFatal, ControlThrowable }
import scala.util.{ Try, Success, Failure }
import scala.runtime.NonLocalReturnControl
import java.util.concurrent.locks.AbstractQueuedSynchronizer
import java.util.concurrent.atomic.AtomicReference
import java.util.Objects.requireNonNull

/**
  * Latch used to implement waiting on a DefaultPromise's result.
  *
  * Inspired by: http://gee.cs.oswego.edu/cgi-bin/viewcvs.cgi/jsr166/src/main/java/util/concurrent/locks/AbstractQueuedSynchronizer.java
  * Written by Doug Lea with assistance from members of JCP JSR-166
  * Expert Group and released to the public domain, as explained at
  * http://creativecommons.org/publicdomain/zero/1.0/
  */
private[impl] final class CompletionLatch[T] extends AbstractQueuedSynchronizer with (Try[T] => Unit) {
  //@volatie not needed since we use acquire/release
  /*@volatile*/ private[this] var _result: Try[T] = null
  final def result: Try[T] = _result
  override protected def tryAcquireShared(ignored: Int): Int = if (getState != 0) 1 else -1
  override protected def tryReleaseShared(ignore: Int): Boolean = {
    setState(1)
    true
  }
  override def apply(value: Try[T]): Unit = {
    _result = value // This line MUST go before releaseShared
    releaseShared(1)
  }
}

private[concurrent] final object Promise {
    /**
     * Link represents a completion dependency between 2 DefaultPromises.
     * As the DefaultPromise referred to by a Link can itself be linked to another promise
     * `relink` traverses such chains and compresses them so that the link always points
     * to the root of the dependency chain.
     *
     * In order to conserve memory, the owner of a Link (a DefaultPromise) is not stored
     * on the Link, but is instead passed in as a parameter to the operation(s).
     *
     * If when compressing a chain of Links it is discovered that the root has been completed,
     * the `owner`'s value is completed with that value, and the Link chain is discarded.
     **/
    private[concurrent] final class Link[T](to: DefaultPromise[T]) extends AtomicReference[DefaultPromise[T]](to) {
      /**
       * Compresses this chain and returns the currently known root of this chain of Links.
       **/
      final def promise(owner: DefaultPromise[T]): DefaultPromise[T] = {
        val c = get()
        relink(current = c, target = c, owner = owner)
      }

      /**
       * The combination of traversing and possibly unlinking of a given `target` DefaultPromise.
       **/
      @inline @tailrec private[this] final def relink(current: DefaultPromise[T], target: DefaultPromise[T], owner: DefaultPromise[T]): DefaultPromise[T] = {
        val value = target.get()
        if (value.isInstanceOf[Link[T]]) relink(current = current, target = value.asInstanceOf[Link[T]].get(), owner = owner)
        else if ((value.isInstanceOf[Callbacks[T]]) && ((current eq target) || (owner eq target) || compareAndSet(current, target))) target
        else if (value.isInstanceOf[Try[T]] && (owner ne null)) {
          unlink(owner = owner, value = value.asInstanceOf[Try[T]])
          owner
        } else relink(current = get(), target = target, owner = owner)
      }


      /**
       * Unlinks (removes) the link chain if the root is discovered to be already completed,
       * and completes the `owner` with that result.
       **/
      @inline @tailrec private[this] final def unlink(owner: DefaultPromise[T], value: Try[T]): Unit = {
        val l = owner.get()
        if (l.isInstanceOf[Link[T]])
          unlink(owner = if (owner.compareAndSet(l, value)) l.asInstanceOf[Link[T]].get() else owner, value = value)
        else if(l.isInstanceOf[Callbacks[T]]) owner.tryComplete(value)
        // The following left implicit: else /* if (l.isInstanceOf[Try[T]]) */ ()
      }
    }

    /**
     * The process of "resolving" a Try is to validate that it only contains
     * those values which makes sense in the context of Futures.
     **/
    // requireNonNull is paramount to guard against null completions
    private[this] final def resolve[T](value: Try[T]): Try[T] =
      if (requireNonNull(value).isInstanceOf[Success[T]]) value
      else {
        val t = value.asInstanceOf[Failure[T]].exception
        if (t.isInstanceOf[ControlThrowable] || t.isInstanceOf[InterruptedException] || t.isInstanceOf[Error]) {
          if (t.isInstanceOf[NonLocalReturnControl[T @unchecked]])
            Success(t.asInstanceOf[NonLocalReturnControl[T]].value)
          else
            Failure(new ExecutionException("Boxed Exception", t))
        } else value
      }

  // Left non-final to enable addition of extra fields by Java/Scala converters in scala-java8-compat.
  class DefaultPromise[T] private[this] (initial: AnyRef) extends AtomicReference[AnyRef](initial) with scala.concurrent.Promise[T] with scala.concurrent.Future[T] {
    /**
     * Constructs a new, completed, Promise.
     */
    final def this(result: Try[T]) = this(resolve(result): AnyRef)

    /**
     * Constructs a new, un-completed, Promise.
     */
    final def this() = this(Noop: AnyRef)

    /**
     * Returns the associaed `Future` with this `Promise`
     */
    override final def future: Future[T] = this

    override final def transform[S](f: Try[T] => Try[S])(implicit executor: ExecutionContext): Future[S] =
      dispatchOrAddCallbacks(get(), new Transformation[T, S](Xform_transform, f, executor))

    override final def transformWith[S](f: Try[T] => Future[S])(implicit executor: ExecutionContext): Future[S] =
      dispatchOrAddCallbacks(get(), new Transformation[T, S](Xform_transformWith, f, executor))

    override final def foreach[U](f: T => U)(implicit executor: ExecutionContext): Unit = {
      val state = get()
      if (!state.isInstanceOf[Failure[T]]) dispatchOrAddCallbacks(state, new Transformation[T, Unit](Xform_foreach, f, executor))
    }

    override final def flatMap[S](f: T => Future[S])(implicit executor: ExecutionContext): Future[S] = {
      val state = get()
      if (!state.isInstanceOf[Failure[T]]) dispatchOrAddCallbacks(state, new Transformation[T, S](Xform_flatMap, f, executor))
      else this.asInstanceOf[Future[S]]
    }

    override final def map[S](f: T => S)(implicit executor: ExecutionContext): Future[S] = {
      val state = get()
      if (!state.isInstanceOf[Failure[T]]) dispatchOrAddCallbacks(state, new Transformation[T, S](Xform_map, f, executor))
      else this.asInstanceOf[Future[S]]
    }

    override final def filter(p: T => Boolean)(implicit executor: ExecutionContext): Future[T] = {
      val state = get()
      if (!state.isInstanceOf[Failure[T]]) dispatchOrAddCallbacks(state, new Transformation[T, T](Xform_filter, p, executor)) // Short-circuit if we get a Success
      else this
    }

    override final def collect[S](pf: PartialFunction[T, S])(implicit executor: ExecutionContext): Future[S] = {
      val state = get()
      if (!state.isInstanceOf[Failure[T]]) dispatchOrAddCallbacks(state, new Transformation[T, S](Xform_collect, pf, executor)) // Short-circuit if we get a Success
      else this.asInstanceOf[Future[S]]
    }

    override final def recoverWith[U >: T](pf: PartialFunction[Throwable, Future[U]])(implicit executor: ExecutionContext): Future[U] = {
      val state = get()
      if (!state.isInstanceOf[Success[T]]) dispatchOrAddCallbacks(state, new Transformation[T, U](Xform_recoverWith, pf, executor)) // Short-circuit if we get a Failure
      else this.asInstanceOf[Future[U]]
    }

    override final def recover[U >: T](pf: PartialFunction[Throwable, U])(implicit executor: ExecutionContext): Future[U] = {
      val state = get()
      if (!state.isInstanceOf[Success[T]]) dispatchOrAddCallbacks(state, new Transformation[T, U](Xform_recover, pf, executor)) // Short-circuit if we get a Failure
      else this.asInstanceOf[Future[U]]
    }

    override final def mapTo[S](implicit tag: scala.reflect.ClassTag[S]): Future[S] =
      if (!get.isInstanceOf[Failure[T]]) super[Future].mapTo[S](tag) // Short-circuit if we get a Success
      else this.asInstanceOf[Future[S]]


    override final def onComplete[U](func: Try[T] => U)(implicit executor: ExecutionContext): Unit =
      dispatchOrAddCallbacks(get(), new Transformation[T, Unit](Xform_onComplete, func, executor))

    override final def failed: Future[Throwable] = {
      val state = get()
      if (!state.isInstanceOf[Success[T]]) super.failed
      else Future.failedFailureFuture // Cached instance in case of already known success
    }

    @tailrec override final def toString: String = {
      val state = get()
      if (state.isInstanceOf[Try[T]]) "Future("+state+")"
      else if (state.isInstanceOf[Link[T]]) state.asInstanceOf[Link[T]].promise(this).toString
      else /*if (state.isInstanceOf[Callbacks[T]]) */ "Future(<not completed>)"
    }

    private[this] final def tryAwait0(atMost: Duration): Try[T] =
      if (atMost ne Duration.Undefined) {
        val v = value0
        if (v ne null) v
        else {
          val r =
            if (atMost <= Duration.Zero) null
            else {
              val l = new CompletionLatch[T]()
              onComplete(l)(InternalCallbackExecutor)

              if (atMost.isFinite)
                l.tryAcquireSharedNanos(1, atMost.toNanos)
              else
                l.acquireSharedInterruptibly(1)

              l.result
            }
          if (r ne null) r
          else throw new TimeoutException("Future timed out after [" + atMost + "]")
        }
      } else throw new IllegalArgumentException("Cannot wait for Undefined duration of time")

    @throws(classOf[TimeoutException])
    @throws(classOf[InterruptedException])
    final def ready(atMost: Duration)(implicit permit: CanAwait): this.type = {
      tryAwait0(atMost)
      this
    }

    @throws(classOf[Exception])
    final def result(atMost: Duration)(implicit permit: CanAwait): T =
      tryAwait0(atMost).get // returns the value, or throws the contained exception

    override final def isCompleted: Boolean = value0 ne null

    override final def value: Option[Try[T]] = Option(value0)

    @tailrec // returns null if not completed
    private final def value0: Try[T] = {
      val state = get()
      if (state.isInstanceOf[Try[T]]) state.asInstanceOf[Try[T]]
      else if (state.isInstanceOf[Link[T]]) state.asInstanceOf[Link[T]].promise(this).value0
      else /*if (state.isInstanceOf[Callbacks[T]])*/ null
    }

    override final def tryComplete(value: Try[T]): Boolean = {
      val state = get()
      if (state.isInstanceOf[Try[T]]) false
      else tryComplete0(state, resolve(value))
    }

    @tailrec // WARNING: important that the supplied Try really is resolve():d
    private[Promise] final def tryComplete0(state: AnyRef, resolved: Try[T]): Boolean =
      if (state.isInstanceOf[Callbacks[T]]) {
        if (compareAndSet(state, resolved)) {
          if (state ne Noop) submitWithValue(state.asInstanceOf[Callbacks[T]], resolved)
          true
        } else tryComplete0(get(), resolved)
      } else if (state.isInstanceOf[Link[T]]) {
        val p = state.asInstanceOf[Link[T]].promise(this) // If this returns owner/this, we are in a completed link
        (p ne this) && p.tryComplete0(p.get(), resolved) // Use this to get tailcall optimization and avoid re-resolution
      } else /* if(state.isInstanceOf[Try[T]]) */ false

    override final def completeWith(other: Future[T]): this.type = {
      val r = if (other.isInstanceOf[DefaultPromise[T]]) other.asInstanceOf[DefaultPromise[T]].value0 else null
      if (r ne null) {
        tryComplete(r)
        this
      } else super.completeWith(other)
    }

    /** Tries to add the callback, if already completed, it dispatches the callback to be executed.
     *  Used by `onComplete()` to add callbacks to a promise and by `link()` to transfer callbacks
     *  to the root promise when linking two promises together.
     */
    @tailrec private final def dispatchOrAddCallbacks[C <: Callbacks[T]](state: AnyRef, callbacks: C): C =
      if (state.isInstanceOf[Try[T]]) {
        submitWithValue(callbacks, state.asInstanceOf[Try[T]])
        callbacks
      } else if (state.isInstanceOf[Callbacks[T]]) {
        if(compareAndSet(state, if (state ne Noop) concatCallbacks(callbacks, state.asInstanceOf[Callbacks[T]]) else callbacks)) callbacks
        else dispatchOrAddCallbacks(get(), callbacks)
      } else /*if (state.isInstanceOf[Link[T]])*/ {
        val p = state.asInstanceOf[Link[T]].promise(this)
        p.dispatchOrAddCallbacks(p.get(), callbacks)
      }

    // IMPORTANT: Noop should never be passed in here, neither as left OR as right
    @tailrec private[this] final def concatCallbacks(left: Callbacks[T], right: Callbacks[T]): Callbacks[T] = {
      if (left.isInstanceOf[Transformation[T,_]]) new ManyCallbacks[T](left, right)
      else /*if (left.isInstanceOf[ManyCallbacks[T]) */ { // This should only happen when linking
        val m = left.asInstanceOf[ManyCallbacks[T]]
        concatCallbacks(m.last, new ManyCallbacks(m.first, right))
      }
    }

    // IMPORTANT: Noop should not be passed in here
    private[this] final def submitWithValue(callbacks: Callbacks[T], resolved: Try[T]): Unit = {
      if (callbacks.isInstanceOf[ManyCallbacks[T]]) {
        val m = callbacks.asInstanceOf[ManyCallbacks[T]]
        if (m.first.isInstanceOf[Transformation[T, _]]) m.first.asInstanceOf[Transformation[T, _]].submitWithValue(resolved)
        else submitWithValue(m.first, resolved) // FIXME this will grow the stack
        submitWithValue(m.last, resolved)
      } else /*if (callbacks ne Noop)*/ {
        callbacks.asInstanceOf[Transformation[T, _]].submitWithValue(resolved)
      }
    }

    /** Link this promise to the root of another promise.
     */
    @tailrec private[concurrent] final def linkRootOf(target: DefaultPromise[T], link: Link[T]): Unit =
      if (this ne target) {
        val state = get()
        if (state.isInstanceOf[Try[T]]) {
          if(!target.tryComplete(state.asInstanceOf[Try[T]]))
            throw new IllegalStateException("Cannot link completed promises together")
        } else if (state.isInstanceOf[Link[T]]) {
          state.asInstanceOf[Link[T]].promise(this).linkRootOf(target, link)
        } else /*if (state.isInstanceOf[Callbacks[T]]) */ {
          val l = if (link ne null) link else new Link(target)
          val p = l.promise(this)
          if (p ne this) {
            if (compareAndSet(state, l)) {
              if (state ne Noop) p.dispatchOrAddCallbacks(p.get(), state.asInstanceOf[Callbacks[T]])
            } else linkRootOf(p, l)
          }
        }
      }
  }

  // Constant byte tags for unpacking transformation function inputs or outputs
  // These need to be Ints to get compiled into constants, but we don't want to
  // pay 32-bit to store them so we convert to/from Byte
  final val Xform_noop          = 0
  final val Xform_map           = 1
  final val Xform_flatMap       = 2
  final val Xform_transform     = 3
  final val Xform_transformWith = 4
  final val Xform_foreach       = 5
  final val Xform_onComplete    = 6
  final val Xform_recover       = 7
  final val Xform_recoverWith   = 8
  final val Xform_filter        = 9
  final val Xform_collect       = 10

    /* Marker trait
   */
  sealed trait Callbacks[-T] {
  }

  final class ManyCallbacks[-T](final val first: Callbacks[T], final val last: Callbacks[T]) extends Callbacks[T] {
    // NOTE: This does grow the stack for *linked* callbacks which are transported across the links,
    // these should normally be rather flat.
    override final def toString: String = "ManyCallbacks"
  }

  private[this] final val Noop = new Transformation[Nothing, Nothing](Xform_noop, null, InternalCallbackExecutor)

  /**
   * A Transformation[F, T] receives an F (it is a Callback[F]) and applies a transformation function to that F,
   * Producing a value of type T (it is a Promise[T]).
   * In order to conserve allocations, indirections, and avoid introducing bi/mega-morphicity the transformation
   * function's type parameters are erased, and the _xform tag will be used to reify them.
   **/
  final class Transformation[-F, T] private[this] (
    private[this] final var _fun: Any => Any,
    private[this] final var _arg: AnyRef,
    private[this] final val _xform: Byte
  ) extends DefaultPromise[T]() with Callbacks[F] with Runnable with Batchable {
    final def this(xform: Int, f: _ => _, ec: ExecutionContext) = this(f.asInstanceOf[Any => Any], ec.prepare(): AnyRef, xform.toByte)

    // Gets invoked when a value is available, schedules it to be run():ed by the ExecutionContext
    // submitWithValue *happens-before* run(), through ExecutionContext.execute.
    // Invariant: _arg is `ExecutionContext`
    // requireNonNull(resolved) will hold as guarded by `resolve`
    final def submitWithValue(resolved: Try[F]): this.type = {
      if (_xform != Xform_noop) {
        val executor = _arg.asInstanceOf[ExecutionContext]
        try {
          _arg = resolved
          executor.execute(this) // Safe publication of _arg = resolved (and _fun)
        } catch {
          case t: Throwable => handleFailure(t, executor)
        }
      }
      this
    }

      private[this] final def handleFailure(t: Throwable, e: ExecutionContext): Unit = {
        _fun = null // allow these to GC
        _arg = null // see above
        val wasInterrupted = t.isInstanceOf[InterruptedException]
        if (NonFatal(t) || wasInterrupted) {
          val completed = tryFailure(t)
          if (completed && wasInterrupted) Thread.currentThread.interrupt()
          if (!completed && (e ne null)) e.reportFailure(t)
        } else throw t
      }

    // Gets invoked by the ExecutionContext, when we have a value to transform.
    // Invariant: if (_arg.isInstanceOf[Try[F]] && (_fun ne null))
    override final def run(): Unit =
        try {
          val v = _arg.asInstanceOf[Try[F]]
          (_xform.toInt: @switch) match {
            case Xform_noop          => doAbort(v)
            case Xform_map           => doMap(v)
            case Xform_flatMap       => doFlatMap(v)
            case Xform_transform     => doTransform(v)
            case Xform_transformWith => doTransformWith(v)
            case Xform_foreach       => doForeach(v)
            case Xform_onComplete    => doOnComplete(v)
            case Xform_recover       => doRecover(v)
            case Xform_recoverWith   => doRecoverWith(v)
            case Xform_filter        => doFilter(v)
            case Xform_collect       => doCollect(v)
            case _                   => doAbort(v)
          }
          _fun = null // allow these to GC
          _arg = null // see above
        } catch {
          case t: Throwable => handleFailure(t, null)
        }

    private[this] final def doMap(v: Try[F]): Unit = tryComplete(v.map(_fun.asInstanceOf[F => T]))

    private[this] final def doFlatMap(v: Try[F]): Unit =
      if (v.isInstanceOf[Success[F]]) {
        val f = _fun(v.asInstanceOf[Success[F]].value)
        if(f.isInstanceOf[DefaultPromise[T]]) f.asInstanceOf[DefaultPromise[T]].linkRootOf(this, null)
        else completeWith(f.asInstanceOf[Future[T]])
      } else tryComplete0(get(), v.asInstanceOf[Try[T]]) // Already resolved

    private[this] final def doTransform(v: Try[F]): Unit = tryComplete(_fun(v).asInstanceOf[Try[T]])

    private[this] final def doTransformWith(v: Try[F]): Unit = {
      val f = _fun(v)
      if(f.isInstanceOf[DefaultPromise[T]]) f.asInstanceOf[DefaultPromise[T]].linkRootOf(this, null)
      else completeWith(f.asInstanceOf[Future[T]])
    }

    private[this] final def doForeach(v: Try[F]): Unit = {
      v foreach _fun
      tryComplete0(get(), Future.successOfUnit.asInstanceOf[Try[T]])  // Already resolved
    }

    private[this] final def doOnComplete(v: Try[F]): Unit = {
      _fun(v)
      tryComplete0(get(), Future.successOfUnit.asInstanceOf[Try[T]]) // Already resolved
    }

    private[this] final def doRecover(v: Try[F]): Unit =
      tryComplete(v.recover(_fun.asInstanceOf[PartialFunction[Throwable, F]]).asInstanceOf[Try[T]]) //recover F=:=T

    private[this] final def doRecoverWith(v: Try[F]): Unit = //recoverWith F=:=T
      if (v.isInstanceOf[Failure[F]]) {
        val f = _fun.asInstanceOf[PartialFunction[Throwable, Future[T]]].applyOrElse(v.asInstanceOf[Failure[F]].exception, Future.recoverWithFailed)
        if (f ne Future.recoverWithFailedMarker) {
           if(f.isInstanceOf[DefaultPromise[T]]) f.asInstanceOf[DefaultPromise[T]].linkRootOf(this, null)
           else completeWith(f)
        } else tryComplete0(get(), v.asInstanceOf[Failure[T]])
      } else tryComplete0(get(), v.asInstanceOf[Try[T]])

    private[this] final def doFilter(v: Try[F]): Unit =
      tryComplete0(get(),
        if (v.isInstanceOf[Failure[F]] || _fun.asInstanceOf[F => Boolean](v.asInstanceOf[Success[F]].value)) v.asInstanceOf[Try[T]]
        else Future.filterFailure // Safe for unresolved completes
      )

    private[this] final def doCollect(v: Try[F]): Unit =
      if (v.isInstanceOf[Success[F]]) tryComplete(Success(_fun.asInstanceOf[PartialFunction[F, T]].applyOrElse(v.asInstanceOf[Success[F]].value, Future.collectFailed)))
      else tryComplete0(get(), v.asInstanceOf[Try[T]]) // Already resolved

    private[this] final def doAbort(v: Try[F]): Unit =
      tryComplete(Failure(new IllegalStateException("BUG: encountered transformation promise with illegal type: " + _xform)))
  }
}
