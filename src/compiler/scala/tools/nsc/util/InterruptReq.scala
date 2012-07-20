package scala.tools.nsc
package util

/** A class of work items to be used in interrupt requests.
 */
abstract class InterruptReq {
  /** The result type of the operation
   */
  type R

  /** The operation to be performed */
  protected val todo: () => R

  /** The result provided */
  private var result: Option[Either[R, Throwable]] = None

  /** To be called from interrupted server to execute demanded task */
  def execute(): Unit = synchronized {
    try {
      result = Some(Left(todo()))
    } catch {
      case t: Throwable => result = Some(Right(t))
    } finally {
      notify()
    }
  }

  /** To be called from interrupting client to get result for interrupt */
  def getResult(): R = synchronized {
    while (result.isEmpty) {
      try {
        wait()
      } catch { case _ : InterruptedException => () }
    }

    result.get match {
      case Left(res) => res
      case Right(t) => throw new FailedInterrupt(t)
    }
  }
}

class FailedInterrupt(cause: Throwable) extends Exception("Compiler exception during call to 'ask'", cause)
