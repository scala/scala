package scala.tools.nsc

import util.ScalaClassLoader
import tools.util.PathResolver
import util.ClassPath.DefaultJavaContext

object ReflectMain extends Driver {

  private def reflectionClassloaderFromSettings(settings: Settings) = {
    val classpath = new PathResolver(settings).result
    ScalaClassLoader.fromURLs(classpath.asURLs, getClass.getClassLoader)
  }

  override def newCompiler(): Global = new ReflectGlobal(settings, reporter, reflectionClassloaderFromSettings(settings))

}