/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 */

package scala.tools.partest
package nest

import scala.tools.nsc.util.ClassPath
import scala.tools.nsc.io.{ Path, File, Directory }
import Path._

object PathSettings {
  import PartestDefaults.{ testRootDir, srcDirName }

  private def cwd = Directory.Current getOrElse sys.error("user.dir property not set")
  private def isPartestDir(d: Directory) = (d.name == "test") && (d / srcDirName isDirectory)
  private def findJar(name: String, ds: Directory*): Either[String, File] =
    ds.toStream flatMap (_.files) filter (_ hasExtension "jar") find ( _.name startsWith name ) map (Right(_)) getOrElse
      Left(s"'${name}.jar' not found in '${ds map (_.path) mkString ", "}'.")

  // Directory <root>/test
  lazy val testRoot: Directory = testRootDir getOrElse {
    val candidates: List[Directory] = (cwd :: cwd.parents) flatMap (d => List(d, Directory(d / "test")))

    candidates find isPartestDir getOrElse sys.error("Directory 'test' not found.")
  }

  // Directory <root>/test/files or .../scaladoc
  lazy val srcDir = Directory(testRoot / srcDirName toCanonical)

  // Directory <root>/build/pack/lib
  lazy val buildPackLibDir = Directory(buildDir / "pack" / "lib")

  // Directory <root>/test/files/lib
  lazy val srcLibDir = Directory(srcDir / "lib")

  // Directory <root>/build
  lazy val buildDir: Directory = {
    val bases      = testRoot :: testRoot.parents
    // In the classic "ant" build, the relevant subdirectory is called build,
    // but in the postmodern "sbt" build, it is called target.  Look for both.
    val dirs = Path.onlyDirs(bases flatMap (x => List(x / "build", x / "target")))

    dirs.headOption getOrElse sys.error("Neither 'build' nor 'target' dir found under test root " + testRoot + ".")
  }


  lazy val srcSpecLib     = findJar("instrumented", Directory(srcDir / "speclib"))
  lazy val srcCodeLib     = findJar("code",  Directory(srcDir / "codelib"), Directory(testRoot / "files" / "codelib") /* work with --srcpath pending */)
  lazy val agentLib       = findJar("scala-partest-javaagent", buildPackLibDir)
  lazy val scalaCheck     = findJar("scalacheck", buildPackLibDir, srcLibDir)
  lazy val testInterface  = findJar("test-interface", buildPackLibDir, srcLibDir)
  lazy val diffUtils      = findJar("diffutils", buildPackLibDir)

  /** The platform-specific support jar, `tools.jar`.
   */
  lazy val platformTools = PathResolver.SupplementalLocations.platformTools
}

class PathSettings() {
  // def classpathAsURLs: List[URL]
}
