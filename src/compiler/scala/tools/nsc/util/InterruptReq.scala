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
  private var result: Option[R] = None

  /** To be called from interrupted server to execute demanded task */
  def execute(): Unit = synchronized {
    result = Some(todo())
    notify()
  }

  /** To be called from interrupting client to get result fo interrupt */
  def getResult(): R = synchronized {
    while (result.isEmpty) wait()
    result.get
  }
}
