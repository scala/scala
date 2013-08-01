package scala.tools.nsc.classpath

import scala.tools.util.PathResolver
import scala.tools.nsc.Settings

object DefaultFlatClasspathManager extends FlatClasspathManager {
  def createClasspath(settings: Settings): FlatClasspath = {
    val wrappedClasspath = new PathResolver(settings).result
    new WrappingFlatClasspath(wrappedClasspath)
  }
}