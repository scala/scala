/*
 * Copyright (c) 2014 Contributor. All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Scala License which accompanies this distribution, and
 * is available at http://www.scala-lang.org/license.html
 */
package scala.tools.nsc.classpath

import scala.reflect.io.AbstractFile
import java.io.{ File => JFile }

/**
 * Common methods related to Java Files and our Abstractfiles used in cotext of classpath
 */
object FileUtils {
  implicit class AbstractFileOps(val file: AbstractFile) extends AnyVal {
    def isPackage = file.isDirectory && isValidPackage(file.name)

    def isClass = !file.isDirectory && file.hasExtension("class")

	  // do we need to check also other files using magic number like in scala.tools.nsc.Jar.isJarOrZip?
	  def isJarOrZip = file.hasExtension("jar") || file.hasExtension("zip")
  }

	implicit class FileOps(val file: JFile) extends AnyVal {
		def isPackage = file.isDirectory && isValidPackage(file.getName)

		def isClass = file.isFile && file.getName.endsWith(".class")
	}

	def dirPath(forPackage: String) = forPackage.replace('.', '/')

	def stripClassExtension(fileName: String) = fileName.substring(0, fileName.length - 6) // equivalent of fileName.length - ".class".length

	private def isValidPackage(dirName: String) = (dirName != "META-INF") && (dirName != "") && (dirName.charAt(0) != '.')
}
