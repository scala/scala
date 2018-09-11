package scala.tools.nsc.jpms

import scala.reflect.internal.FatalError
import scala.tools.nsc.Settings
import scala.tools.nsc.util.ClassPath

// Fallback for when compiler-jdk11 is not available (when compiling on older jdks)
object JpmsClassPath {
  def apply(s: Settings, classPath: String): ClassPath = {
    throw new FatalError("This functionality is only available in the jdk11 preview builds.")
  }
}
