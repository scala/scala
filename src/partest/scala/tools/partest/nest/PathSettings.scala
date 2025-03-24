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

package scala.tools.partest
package nest

import scala.tools.nsc.io.{Directory, File}
import scala.tools.nsc.Properties.propOrNone

/** Get current value for path settings.
  * Default values are read from system properties `partest.srcdir` and `partest.root`.
  */
class PathSettings(testSourcePath: String) {

  // defaults can be set using the environment
  private[this] val defaultTestRootName  = propOrNone("partest.root")

  private[this] def cwd = Directory.Current getOrElse sys.error("user.dir property not set")
  private[this] def isPartestDir(d: Directory) = (d.name == "test") && ((d / testSourcePath).isDirectory)
  private[this] def findJar(name: String, ds: Directory*): Either[String, File] =
    ds.iterator flatMap (_.files) filter (_ hasExtension "jar") find ( _.name startsWith name ) map (Right(_)) getOrElse
      Left(s"'${name}.jar' not found in '${ds map (_.path) mkString ", "}'.")

  // Directory <root>/test
  val testRoot: Directory = (defaultTestRootName map (Directory(_))) getOrElse {
    val candidates: List[Directory] = (cwd :: cwd.parents) flatMap (d => List(d, Directory(d / "test")))

    candidates find isPartestDir getOrElse sys.error("Directory 'test' not found.")
  }

  // Directory <root> (aka the repo root)
  val testParent = testRoot.parent

  // Directory <root>/test/files or .../scaladoc
  val srcDir = Directory((testRoot / testSourcePath).toCanonical)

  def srcSpecLib     = findJar("instrumented", Directory(srcDir / "speclib"))
}
