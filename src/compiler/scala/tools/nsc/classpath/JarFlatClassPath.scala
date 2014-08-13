/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import scala.reflect.io.FileZipArchive
import java.io.File

case class JarFlatClassPath(val jarFile: File) extends AbstractFileFlatClassPath(new FileZipArchive(jarFile))
