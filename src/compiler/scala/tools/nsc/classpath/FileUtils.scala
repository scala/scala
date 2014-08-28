/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import java.io.{ File => JFile }
import java.net.URL
import scala.reflect.io.AbstractFile
import scala.reflect.internal.FatalError

/**
 * Common methods related to Java Files and our Abstractfiles used in cotext of classpath
 */
object FileUtils {
  implicit class AbstractFileOps(val file: AbstractFile) extends AnyVal {
    def isPackage: Boolean = file.isDirectory && isValidPackage(file.name)

    def isClass: Boolean = !file.isDirectory && file.hasExtension("class")

    def isScalaOrJavaSource: Boolean = !file.isDirectory && (file.hasExtension("scala") || file.hasExtension("java"))

	  // TODO do we need to check also other files using magic number like in scala.tools.nsc.Jar.isJarOrZip?
	  def isJarOrZip: Boolean = file.hasExtension("jar") || file.hasExtension("zip")

    /**
     * Safe method returning sequence containing one URL representing this file, when underlying file exists,
     * and returning given default value in other case
     */
    def toURLs(default: => Seq[URL] = Seq.empty): Seq[URL] = if (file.file == null) default else Seq(file.toURL)
  }

  implicit class FileOps(val file: JFile) extends AnyVal {
    def isPackage: Boolean = file.isDirectory && isValidPackage(file.getName)

    def isClass: Boolean = file.isFile && file.getName.endsWith(".class")
  }

  def stripSourceExtension(fileName: String): String = {
    if (endsScala(fileName)) stripClassExtension(fileName)
    else if (endsJava(fileName)) stripJavaExtension(fileName)
    else throw new FatalError("Unexpected source file ending: " + fileName)
  }

  @inline def dirPath(forPackage: String) = forPackage.replace('.', '/')

  @inline def endsClass(fileName: String): Boolean =
    fileName.length > 6 && fileName.substring(fileName.length - 6) == ".class"

  @inline def endsScalaOrJava(fileName: String): Boolean =
    endsScala(fileName) || endsJava(fileName)

  @inline def endsJava(fileName: String): Boolean =
    fileName.length > 5 && fileName.substring(fileName.length - 5) == ".java"

  @inline def endsScala(fileName: String): Boolean =
    fileName.length > 6 && fileName.substring(fileName.length - 6) == ".scala"

  @inline def stripClassExtension(fileName: String): String =
    fileName.substring(0, fileName.length - 6) // equivalent of fileName.length - ".class".length

  @inline def stripJavaExtension(fileName: String): String =
    fileName.substring(0, fileName.length - 5)

  private def isValidPackage(dirName: String): Boolean =
    (dirName != "META-INF") && (dirName != "") && (dirName.charAt(0) != '.')
}
