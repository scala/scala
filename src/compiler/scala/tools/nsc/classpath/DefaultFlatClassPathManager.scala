package scala.tools.nsc.classpath

import scala.tools.nsc.Settings
import scala.tools.util.FlatClassPathResolver

object DefaultFlatClassPathManager extends FlatClassPathManager {
  def createClassPath(settings: Settings): FlatClassPath = {
    val flatClassPathFactory = new FlatClassPathFactory
    val flatClassPathResolver = new FlatClassPathResolver(settings, flatClassPathFactory)
    val aggregateFlatClassPath = new AggregateFlatClassPath(flatClassPathResolver.containers)
    aggregateFlatClassPath
  }
}
