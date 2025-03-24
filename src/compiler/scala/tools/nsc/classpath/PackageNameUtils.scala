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

import scala.tools.nsc.util.ClassPath.RootPackage

/**
 * Common methods related to package names represented as String
 */
object PackageNameUtils {

  /**
   * @param fullClassName full class name with package
   * @return (package, simple class name)
   */
  @inline def separatePkgAndClassNames(fullClassName: String): (String, String) = {
    val lastDotIndex = fullClassName.lastIndexOf('.')
    if (lastDotIndex == -1)
      (RootPackage, fullClassName)
    else
      (fullClassName.substring(0, lastDotIndex), fullClassName.substring(lastDotIndex + 1))
  }

  def packagePrefix(inPackage: String): String = if (inPackage == RootPackage) "" else inPackage + "."

  /**
   * `true` if `packageDottedName` is a package directly nested in `inPackage`, for example:
   *   - `packageContains("scala", "scala.collection")`
   *   - `packageContains("", "scala")`
   */
  def packageContains(inPackage: String, packageDottedName: String) = {
    if (packageDottedName.contains("."))
      packageDottedName.startsWith(inPackage) && packageDottedName.lastIndexOf('.') == inPackage.length
    else inPackage == ""
  }
}
