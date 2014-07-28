/*
 * Copyright (c) 2014 Contributor. All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Scala License which accompanies this distribution, and
 * is available at http://www.scala-lang.org/license.html
 */
package scala.tools.nsc.classpath

import scala.tools.nsc.Settings
import scala.tools.util.FlatClasspathResolver

object DefaultFlatClasspathManager extends FlatClasspathManager {
  override def createClasspath(settings: Settings): FlatClasspath = {
    val flatClasspathFactory = new FlatClasspathFactory
    val flatClasspathResolver = new FlatClasspathResolver(settings, flatClasspathFactory)
    new AggregateFlatClasspath(flatClasspathResolver.containers)
  }
}
