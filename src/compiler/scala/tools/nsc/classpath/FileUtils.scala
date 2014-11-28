/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import java.net.URL
import scala.reflect.internal.FatalError
import scala.reflect.io.AbstractFile

/**
 * Common methods related to files used in the context of classpath
 */
object FileUtils {
  implicit class AbstractFileOps(val file: AbstractFile) extends AnyVal {

    /**
     * Safe method returning a sequence containing one URL representing this file, when underlying file exists,
     * and returning given default value in other case
     */
    def toURLs(default: => Seq[URL] = Seq.empty): Seq[URL] = if (file.file == null) default else Seq(file.toURL)
  }

  def stripSourceExtension(fileName: String): String = {
    if (endsScala(fileName)) stripClassExtension(fileName)
    else if (endsJava(fileName)) stripJavaExtension(fileName)
    else throw new FatalError("Unexpected source file ending: " + fileName)
  }

  def endsClass(fileName: String): Boolean =
    fileName.length > 6 && fileName.substring(fileName.length - 6) == ".class"

  def endsScalaOrJava(fileName: String): Boolean =
    endsScala(fileName) || endsJava(fileName)

  def endsJava(fileName: String): Boolean =
    fileName.length > 5 && fileName.substring(fileName.length - 5) == ".java"

  def endsScala(fileName: String): Boolean =
    fileName.length > 6 && fileName.substring(fileName.length - 6) == ".scala"

  def stripClassExtension(fileName: String): String =
    fileName.substring(0, fileName.length - 6) // equivalent of fileName.length - ".class".length

  def stripJavaExtension(fileName: String): String =
    fileName.substring(0, fileName.length - 5)
}
