package scala.tools.nsc
package interactive

import scala.concurrent.SyncVar

/** Typical interaction, given a predicate <user-input>, a function <display>,
 *  and an exception handler <handle>:
 *
 *  val TIMEOUT = 100 // (milliseconds) or something like that
 *  val r = new Response()
 *  while (!r.isComplete && !r.isCancelled) {
 *    if (<user-input>) r.cancel()
 *    else r.get(TIMEOUT) match {
 *      case Some(Left(data)) => <display>(data)
 *      case Some(Right(exc)) => <handle>(exc)
 *      case None =>
 *    }
 *  }
 */
class Response[T] {

  private val data = new SyncVar[Either[T, Throwable]]
  private var complete = false
  private var cancelled = false

  /** Set provisional data, more to come
   */
  def setProvisionally(x: T) =
    data.set(Left(x))

  /** Set final data, and mark resposne as complete.
   */
  def set(x: T) = data.synchronized {
    data.set(Left(x))
    complete = true
  }

  def raise(exc: Throwable) = data.synchronized {
    data.set(Right(exc))
    complete = true
  }

  /** Get data, wait as long as necessary
   */
  def get: Either[T, Throwable] = data.get

  /** Optionally get data within `timeout` milliseconds.
   */
  def get(timeout: Long): Option[Either[T, Throwable]] = data.get(timeout)

  /** Final data set was stored
   */
  def isComplete = data.synchronized { complete }

  /** Cancel action computing this response
   */
  def cancel() = data.synchronized { cancelled = true }

  /** A cancel request for this response has been issued
   */
  def isCancelled = data.synchronized { cancelled }

  def clear() = data.synchronized {
    data.unset()
    complete = false
    cancelled = false
  }
}
