package scala.tools
package reflect

import scala.tools.nsc.Driver
import scala.tools.nsc.Global
import scala.tools.nsc.Settings
import scala.tools.nsc.util.ClassPath.DefaultJavaContext
import scala.tools.nsc.util.ScalaClassLoader
import scala.tools.util.PathResolver

object ReflectMain extends Driver {

  private def classloaderFromSettings(settings: Settings) = {
    val classpath = new PathResolver(settings).result
    ScalaClassLoader.fromURLs(classpath.asURLs, getClass.getClassLoader)
  }

  override def newCompiler(): Global = new ReflectGlobal(settings, reporter, classloaderFromSettings(settings))
}