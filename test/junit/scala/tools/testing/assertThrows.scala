package scala.tools
package testing

import org.junit.Assert._

object assertThrows {
  def apply[T <: AnyRef](f: => Any)(implicit manifest: Manifest[T]): Unit = {
    val clazz = manifest.erasure.asInstanceOf[Class[T]]
    val thrown =
      try {
        f
        false
      } catch {
        case u: Throwable =>
          if (!clazz.isAssignableFrom(u.getClass))
            assertTrue(s"wrong exception: $u", false)
          true
      }
    if(!thrown)
      assertTrue("exception wasn't thrown", false)
  }
}