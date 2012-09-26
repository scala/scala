package scala.reflect.macros
package runtime

import scala.tools.nsc.util.ScalaClassLoader

trait Infrastructure {
  self: Context =>

  val currentRun: Run = universe.currentRun

  val currentClassPath: List[java.net.URL] = universe.classPath.asURLs

}
