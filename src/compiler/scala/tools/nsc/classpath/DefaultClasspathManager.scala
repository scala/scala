package scala.tools.nsc.classpath

import scala.tools.util.PathResolver
import scala.tools.nsc.Settings
import scala.tools.util.FlatClasspathResolver

object DefaultFlatClasspathManager extends FlatClasspathManager {
  def createClasspath(settings: Settings): FlatClasspath = {
    val flatClasspathFactory = new FlatClasspathFactory
    val flatClasspathResolver = new FlatClasspathResolver(settings, flatClasspathFactory)
    val aggregateFlatClasspath = new AggregateFlatClasspath(flatClasspathResolver.containers)
    aggregateFlatClasspath
  }
}
