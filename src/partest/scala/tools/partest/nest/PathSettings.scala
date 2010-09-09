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
  import PartestDefaults.{ testRootDir, srcDirName }

  private def cwd = Directory.Current getOrElse error("user.dir property not set")
  private def isPartestDir(d: Directory) = (d.name == "test") && (d / srcDirName isDirectory)

  // Directory <root>/test
  lazy val testRoot: Directory = testRootDir getOrElse {
    val candidates: List[Directory] = (cwd :: cwd.parents) flatMap (d => List(d, Directory(d / "test")))

    candidates find isPartestDir getOrElse error("Directory 'test' not found.")
  }

  // Directory <root>/test/files
  lazy val srcDir = Directory(testRoot / srcDirName normalize)

  // Directory <root>/test/files/lib
  lazy val srcLibDir = Directory(srcDir / "lib")

  // Directory <root>/build
  lazy val buildDir = Directory("build")

  // Directory <root>/build/pack/lib
  lazy val buildPackLibDir = Directory(buildDir / "pack" / "lib")

  lazy val scalaCheck = {
    (buildPackLibDir.files find (_.name startsWith "scalacheck")) orElse
    (srcLibDir.files find (_.name startsWith "scalacheck")) getOrElse {
      error("No scalacheck jar found in '%s' or '%s'".format(buildPackLibDir, srcLibDir))
    }
  }
}

class PathSettings() {
  // def classpathAsURLs: List[URL]
}
