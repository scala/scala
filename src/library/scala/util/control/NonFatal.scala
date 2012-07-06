/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.util.control

/**
 * Extractor of non-fatal Throwables. Will not match fatal errors like `VirtualMachineError`
 * (for example, `OutOfMemoryError`, a subclass of `VirtualMachineError`), `ThreadDeath`,
 * `LinkageError`, `InterruptedException`, `ControlThrowable`, or `NotImplementedError`.
 * However, `StackOverflowError` is matched, i.e. considered non-fatal.
 *
 * Note that [[scala.util.control.ControlThrowable]], an internal Throwable, is not matched by
 * `NonFatal` (and would therefore be thrown).
 *
 * For example, all harmless Throwables can be caught by:
 * {{{
 *   try {
 *     // dangerous stuff
 *   } catch {
 *     case NonFatal(e) => log.error(e, "Something not that bad.")
 *   }
 * }}}
 */
object NonFatal {

  def unapply(t: Throwable): Option[Throwable] = t match {
    case e: StackOverflowError ⇒ Some(e) // StackOverflowError ok even though it is a VirtualMachineError
    // VirtualMachineError includes OutOfMemoryError and other fatal errors
    case _: VirtualMachineError | _: ThreadDeath | _: InterruptedException | _: LinkageError | _: ControlThrowable | _: NotImplementedError => None
    case e ⇒ Some(e)
  }

}
