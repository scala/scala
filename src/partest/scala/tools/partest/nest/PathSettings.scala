/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 */

package scala.tools.partest
package nest

import scala.tools.nsc.Properties.{ setProp, propOrEmpty, propOrNone, propOrElse }
import scala.tools.nsc.util.ClassPath
import scala.tools.nsc.io
import io.{ Path, File, Directory }
import RunnerUtils._
import java.net.URLClassLoader

object PathSettings {
  private def isTestDirectory(d: Path) =
    d.isDirectory && (d.name == "test") && (d / srcDirProperty isDirectory)

  def testRootProperty  = propOrNone("scalatest.root") map (x => Directory(x))
  def srcDirProperty    = propOrElse("partest.srcdir", "files")

  // XXX temp
  def prefixDir = Directory.Current getOrElse error("user.dir property not set")

  lazy val testRoot: Directory = testRootProperty getOrElse {
    // val cwd = Directory.Current getOrElse error("user.dir property not set")
    val cwd = prefixDir
    val candidates = (cwd :: cwd.parents) flatMap (d => List(d, d / "test"))

    candidates find isTestDirectory map (_.toDirectory) getOrElse error("Directory 'test' not found.")
  }

  lazy val srcDir = Directory((testRoot / srcDirProperty).normalize.toAbsolute)
}

class PathSettings() {
  // def classpathAsURLs: List[URL]
}
