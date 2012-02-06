/* NEST (New Scala Test)
 * Copyright 2007-2011 LAMP/EPFL
 */

package scala.tools.partest
package nest

import scala.tools.nsc.Properties.{ setProp, propOrEmpty, propOrNone, propOrElse }
import scala.tools.nsc.util.ClassPath
import scala.tools.nsc.io
import io.{ Path, File, Directory }
import RunnerUtils._

object PathSettings {
  import PartestDefaults.{ testRootDir, srcDirName }

  private def cwd = Directory.Current getOrElse sys.error("user.dir property not set")
  private def isPartestDir(d: Directory) = (d.name == "test") && (d / srcDirName isDirectory)
  private def findJar(d: Directory, name: String): Option[File] = findJar(d.files, name)
  private def findJar(files: Iterator[File], name: String): Option[File] =
    files filter (_ hasExtension "jar") find { _.name startsWith name }

  // Directory <root>/test
  lazy val testRoot: Directory = testRootDir getOrElse {
    val candidates: List[Directory] = (cwd :: cwd.parents) flatMap (d => List(d, Directory(d / "test")))

    candidates find isPartestDir getOrElse sys.error("Directory 'test' not found.")
  }

  // Directory <root>/test/files
  lazy val srcDir = Directory(testRoot / srcDirName toCanonical)

  // Directory <root>/test/files/lib
  lazy val srcLibDir = Directory(srcDir / "lib")

  // Directory <root>/test/files/speclib
  lazy val srcSpecLibDir = Directory(srcDir / "speclib")

  lazy val srcSpecLib: File = findJar(srcSpecLibDir, "instrumented") getOrElse {
    sys.error("No instrumented.jar found in %s".format(srcSpecLibDir))
  }

  // Directory <root>/test/files/codelib
  lazy val srcCodeLibDir = Directory(srcDir / "codelib")

  lazy val srcCodeLib: File = findJar(srcCodeLibDir, "code") getOrElse {
    sys.error("No code.jar found in %s".format(srcCodeLibDir))
  }

  // Directory <root>/build
  lazy val buildDir: Directory = {
    val bases      = testRoot :: testRoot.parents
    // In the classic "ant" build, the relevant subdirectory is called build,
    // but in the postmodern "sbt" build, it is called target.  Look for both.
    val dirs = Path.onlyDirs(bases flatMap (x => List(x / "build", x / "target")))

    dirs.headOption getOrElse sys.error("Neither 'build' nor 'target' dir found under test root " + testRoot + ".")
  }

  // Directory <root>/build/pack/lib
  lazy val buildPackLibDir = Directory(buildDir / "pack" / "lib")

  lazy val scalaCheck: File =
    findJar(buildPackLibDir.files ++ srcLibDir.files, "scalacheck") getOrElse {
      sys.error("No scalacheck jar found in '%s' or '%s'".format(buildPackLibDir, srcLibDir))
    }
}

class PathSettings() {
  // def classpathAsURLs: List[URL]
}
