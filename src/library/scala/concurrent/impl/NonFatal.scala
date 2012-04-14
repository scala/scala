/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent
package impl

/**
 * Extractor of non-fatal Throwables. Will not match fatal errors
 * like VirtualMachineError (OutOfMemoryError)
 * ThreadDeath, LinkageError and InterruptedException.
 * StackOverflowError is matched, i.e. considered non-fatal.
 *
 * Usage to catch all harmless throwables:
 * {{{
 *   try {
 *     // dangerous stuff
 *   } catch {
 *     case NonFatal(e) => log.error(e, "Something not that bad")
 *   }
 * }}}
 */
private[concurrent] object NonFatal {

  def unapply(t: Throwable): Option[Throwable] = t match {
    case e: StackOverflowError ⇒ Some(e) // StackOverflowError ok even though it is a VirtualMachineError
    // VirtualMachineError includes OutOfMemoryError and other fatal errors
    case _: VirtualMachineError | _: ThreadDeath | _: InterruptedException | _: LinkageError ⇒ None
    case e ⇒ Some(e)
  }

}

