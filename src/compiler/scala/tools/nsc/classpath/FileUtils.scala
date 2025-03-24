/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.classpath

import java.io.{File => JFile, FileFilter}
import java.net.URL
import scala.reflect.internal.FatalError
import scala.reflect.io.{AbstractFile, ZipArchive}

/**
 * Common methods related to Java files and abstract files used in the context of classpath
 */
object FileUtils {
  implicit class AbstractFileOps(val file: AbstractFile) extends AnyVal {
    def isPackage: Boolean = file.isDirectory && mayBeValidPackage(file.name)

    def isClass: Boolean = !file.isDirectory && (file.hasExtension("class") || file.hasExtension("sig") || file.hasExtension("tasty"))

    def isScalaOrJavaSource: Boolean = !file.isDirectory && (file.hasExtension("scala") || file.hasExtension("java"))

    def isJarOrZip: Boolean = file.isInstanceOf[ZipArchive] || !file.isDirectory && (file.hasExtension("jar") || file.hasExtension("zip"))

    /**
     * Safe method returning a sequence containing one URL representing this file, when underlying file exists,
     * and returning given default value in other case
     */
    def toURLs(default: => Seq[URL] = Seq.empty): Seq[URL] = if (file.file == null) default else Seq(file.toURL)
  }

  implicit class FileOps(val file: JFile) extends AnyVal {
    def isPackage: Boolean = file.isDirectory && mayBeValidPackage(file.getName)

    def isClass: Boolean = file.isFile && endsClass(file.getName)
  }
  private val SUFFIX_CLASS = ".class"
  private val SUFFIX_SCALA = ".scala"
  private val SUFFIX_JAVA = ".java"
  private val SUFFIX_SIG = ".sig"
  private val SUFFIX_TASTY = ".tasty"

  def stripSourceExtension(fileName: String): String = {
    if (endsScala(fileName)) stripClassExtension(fileName)
    else if (endsJava(fileName)) stripJavaExtension(fileName)
    else throw FatalError("Unexpected source file ending: " + fileName)
  }

  def dirPath(forPackage: String) = forPackage.replace('.', '/')

  @inline private def ends (filename:String, suffix:String) = filename.endsWith(suffix) && filename.length > suffix.length

  def classNameToTasty(fileName: String): String = {
    // TODO [tasty]: Dotty really wants to special-case standalone objects
    // i.e. their classfile will end with `$`, but the tasty file will not.
    //   however then it needs to escape `Null$`, `Nothing$`, and `$`
    //   because these are "legitimate" classes with `$` in their name.
    // It seems its not actually necessary to drop these files,
    //   as the classfile parser will not complain about them,
    //   however, it could increase efficiency to follow dotty
    //   and drop them anyway.
    // Scala 3 also prevents compilation of `object Foo` and `class Foo$` in the same package
    // See test/tasty/run/src-2/tastytest/TestRuntimeSpecialClasses.scala for a test case
    val isStandaloneObjectHeuristic = (
      fileName.lastIndexOf('$') == fileName.length - 7
        && fileName != "Null$.class"
        && fileName != "Nothing$.class"
        && fileName != "$.class"
    )
    val className =
      if (isStandaloneObjectHeuristic)
        fileName.stripSuffix("$.class")
      else
        fileName.stripSuffix(".class")
    className + SUFFIX_TASTY
  }

  def endsClass(fileName: String): Boolean =
    ends (fileName, SUFFIX_CLASS) || fileName.endsWith(SUFFIX_SIG) || fileName.endsWith(SUFFIX_TASTY)

  def endsScalaOrJava(fileName: String): Boolean =
    endsScala(fileName) || endsJava(fileName)

  def endsJava(fileName: String): Boolean =
    ends (fileName, SUFFIX_JAVA)

  def endsScala(fileName: String): Boolean =
    ends (fileName, SUFFIX_SCALA)

  def stripClassExtension(fileName: String): String =
    fileName.substring(0, fileName.lastIndexOf('.'))

  def stripJavaExtension(fileName: String): String =
    fileName.substring(0, fileName.length - 5) // equivalent of fileName.length - SUFFIX_JAVA.length

  // probably it should match a pattern like [a-z_]{1}[a-z0-9_]* but it cannot be changed
  // because then some tests in partest don't pass
  def mayBeValidPackage(dirName: String): Boolean =
    (dirName != "META-INF") && (dirName != "") && (dirName.charAt(0) != '.')

  def mkFileFilter(f: JFile => Boolean) = new FileFilter {
    def accept(pathname: JFile): Boolean = f(pathname)
  }
}
