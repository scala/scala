/*
 * Copyright (c) 2014 Contributor. All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Scala License which accompanies this distribution, and
 * is available at http://www.scala-lang.org/license.html
 */
package scala.tools.nsc.classpath

import scala.tools.nsc.Settings
import scala.tools.util.FlatClassPathResolver

object DefaultFlatClassPathManager extends FlatClassPathManager {
  override def createClassPath(settings: Settings): FlatClassPath = {
    val flatClassPathFactory = new FlatClassPathFactory(settings)
    val flatClassPathResolver = new FlatClassPathResolver(settings, flatClassPathFactory)
    new AggregateFlatClassPath(flatClassPathResolver.containers)
  }
}
