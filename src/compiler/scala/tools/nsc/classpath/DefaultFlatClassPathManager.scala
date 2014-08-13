/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import scala.tools.nsc.Settings
import scala.tools.util.FlatClassPathResolver

object DefaultFlatClassPathManager extends FlatClassPathManager {
  override def createClassPath(settings: Settings): FlatClassPath = {
    val flatClassPathFactory = new FlatClassPathFactory(settings)
    val flatClassPathResolver = new FlatClassPathResolver(settings, flatClassPathFactory)
    flatClassPathResolver.result
  }
}
