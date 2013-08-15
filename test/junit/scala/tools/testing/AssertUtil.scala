package scala.tools
package testing

/** This module contains additional higher-level assert statements
 *  that are ultimately based on junit.Assert primitives.
 */
object AssertUtil {
  /** Check if exception T (or a subclass) was thrown during evaluation of f.
   *  If any other exception or throwable is found instead it will be re-thrown.
   */
  def assertThrows[T <: Exception](f: => Any)(implicit manifest: Manifest[T]): Unit =
    try f
    catch {
      case e: Exception =>
        val clazz = manifest.erasure.asInstanceOf[Class[T]]
        if (!clazz.isAssignableFrom(e.getClass))
          throw e
    }
}