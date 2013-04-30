/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 */

package scala.tools.partest
package nest

import scala.tools.nsc.Properties.{ setProp, propOrEmpty, propOrNone, propOrElse }
import scala.tools.nsc.util.ClassPath
import scala.tools.nsc.io.{ Path, File, Directory }
import scala.util.Properties.{ envOrElse, envOrNone, javaHome, jdkHome }
import Path._

object PathSettings {
  import PartestDefaults.{ testRootDir, srcDirName }

  private def cwd = Directory.Current getOrElse sys.error("user.dir property not set")
  private def isPartestDir(d: Directory) = (d.name == "test") && (d / srcDirName isDirectory)
  private def findJar(d: Directory, name: String): Option[File] = findJar(d.files, name)
  private def findJar(files: Iterator[File], name: String): Option[File] =
    files filter (_ hasExtension "jar") find { _.name startsWith name }
  private def findJarOrFail(name: String, ds: Directory*): File = findJar(ds flatMap (_.files) iterator, name) getOrElse
    sys.error(s"'${name}.jar' not found in '${ds map (_.path) mkString ", "}'.")

  // Directory <root>/test
  lazy val testRoot: Directory = testRootDir getOrElse {
    val candidates: List[Directory] = (cwd :: cwd.parents) flatMap (d => List(d, Directory(d / "test")))

    candidates find isPartestDir getOrElse sys.error("Directory 'test' not found.")
  }

  // Directory <root>/test/files or .../scaladoc
  def srcDir = Directory(testRoot / srcDirName toCanonical)

  // Directory <root>/test/files/lib
  lazy val srcLibDir = Directory(srcDir / "lib")

  // Directory <root>/test/files/speclib
  lazy val srcSpecLibDir = Directory(srcDir / "speclib")

  lazy val srcSpecLib: File = findJar(srcSpecLibDir, "instrumented") getOrElse {
    sys.error("No instrumented.jar found in %s".format(srcSpecLibDir))
  }

  // Directory <root>/test/files/codelib
  lazy val srcCodeLibDir = Directory(srcDir / "codelib")

  lazy val srcCodeLib: File = (
    findJar(srcCodeLibDir, "code")
      orElse findJar(Directory(testRoot / "files" / "codelib"), "code") // work with --srcpath pending
      getOrElse sys.error("No code.jar found in %s".format(srcCodeLibDir))
  )

  lazy val instrumentationAgentLib: File = {
    findJar(buildPackLibDir.files, "scala-partest-javaagent") getOrElse {
      sys.error("No partest-javaagent jar found in '%s' or '%s'".format(buildPackLibDir, srcLibDir))
    }
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

  lazy val testInterface: File = findJarOrFail("test-interface", buildPackLibDir, srcLibDir)

  lazy val diffUtils: File =
    findJar(buildPackLibDir.files, "diffutils") getOrElse sys.error(s"No diffutils.jar found in '$buildPackLibDir'.")

  /** The platform-specific support jar.
   *  Usually this is tools.jar in the jdk/lib directory of the platform distribution.
   *  The file location is determined by probing the lib directory under JDK_HOME or JAVA_HOME,
   *  if one of those environment variables is set, then the lib directory under java.home,
   *  and finally the lib directory under the parent of java.home. Or, as a last resort,
   *  search deeply under those locations (except for the parent of java.home, on the notion
   *  that if this is not a canonical installation, then that search would have little
   *  chance of succeeding).
   */
  lazy val platformTools: Option[File] = {
    val jarName = "tools.jar"
    def jarPath(path: Path) = (path / "lib" / jarName).toFile
    def jarAt(path: Path) = {
      val f = jarPath(path)
      if (f.isFile) Some(f) else None
    }
    val jdkDir = {
      val d = Directory(jdkHome)
      if (d.isDirectory) Some(d) else None
    }
    def deeply(dir: Directory) = dir.deepFiles find (_.name == jarName)

    val home    = envOrNone("JDK_HOME") orElse envOrNone("JAVA_HOME") map (p => Path(p))
    val install = Some(Path(javaHome))

    (home flatMap jarAt) orElse (install flatMap jarAt) orElse (install map (_.parent) flatMap jarAt) orElse
      (jdkDir flatMap deeply)
  }
}

class PathSettings() {
  // def classpathAsURLs: List[URL]
}
