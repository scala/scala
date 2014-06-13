/*
 * Copyright (c) 2014 Contributor. All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Scala License which accompanies this distribution, and
 * is available at http://www.scala-lang.org/license.html
 */
package scala.tools.nsc.classpath

import scala.reflect.io.FileZipArchive
import java.io.File

case class JarFlatClasspath(val jarFile: File) extends AbstractFileFlatClasspath(new FileZipArchive(jarFile))
