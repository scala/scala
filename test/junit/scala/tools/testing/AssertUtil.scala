package scala.tools
package testing

/** This module contains additional higher-level assert statements
 *  that are ultimately based on junit.Assert primitives.
 */
object AssertUtil {
  /**
   * Check if throwable T (or a subclass) was thrown during evaluation of f, and that its message
   * satisfies the `checkMessage` predicate.
   * If any other exception will be re-thrown.
   */
  def assertThrows[T <: Throwable](f: => Any,
                                   checkMessage: String => Boolean = s => true)
                                  (implicit manifest: Manifest[T]): Unit = {
    try f
    catch {
      case e: Throwable if checkMessage(e.getMessage) =>
        val clazz = manifest.runtimeClass
        if (!clazz.isAssignableFrom(e.getClass))
          throw e
    }
  }
}
