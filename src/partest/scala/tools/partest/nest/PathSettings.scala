/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 */

package scala.tools.partest
package nest

import scala.tools.nsc.util.ClassPath
import scala.tools.nsc.io.{ Path, File, Directory }
import scala.tools.nsc.Properties.{ propOrElse, propOrNone }
import Path._

/** Get current value for path settings -- these depend on the mutable `testSourcePath`.
 * Default values are read from system properties `partest.srcdir` and `partest.root`.
 *
 * TODO: make `testSourcePath` immutable, but that's pretty involved as a lot of stuff depends on it (all the more reason to refactor)
 *     (we don't use system properties to configure partest internally,
 *      as their behavior depends on the security policy in place -- e.g., ant does not allow overwriting properties)
 *
 * NOTE: the members are methods because `testSourcePath` changes.
 */
object PathSettings {
  private[nest] var testSourcePath: String = null // set by RunnerSuite

  // defaults can be set using the environment, but note PathSettings is mutable
  private def defaultTestRootName  = propOrNone("partest.root")

  private def cwd = Directory.Current getOrElse sys.error("user.dir property not set")
  private def isPartestDir(d: Directory) = (d.name == "test") && (d / testSourcePath isDirectory)
  private def findJar(name: String, ds: Directory*): Either[String, File] =
    ds.toStream flatMap (_.files) filter (_ hasExtension "jar") find ( _.name startsWith name ) map (Right(_)) getOrElse
      Left(s"'${name}.jar' not found in '${ds map (_.path) mkString ", "}'.")

  // Directory <root>/test
  def testRoot: Directory = (defaultTestRootName map (Directory(_))) getOrElse {
    val candidates: List[Directory] = (cwd :: cwd.parents) flatMap (d => List(d, Directory(d / "test")))

    candidates find isPartestDir getOrElse sys.error("Directory 'test' not found.")
  }
  def testParent = testRoot.parent

  // Directory <root>/test/files or .../scaladoc
  def srcDir = Directory(testRoot / testSourcePath toCanonical)

  // Directory <root>/build/pack/lib
  private def buildPackLibDir = Directory(buildDir / "pack" / "lib")

  // Directory <root>/test/files/lib
  private def srcLibDir = Directory(srcDir / "lib")

  // Directory <root>/build
  private def buildDir: Directory = {
    val bases      = testRoot :: testRoot.parents
    // In the classic "ant" build, the relevant subdirectory is called build,
    // but in the postmodern "sbt" build, it is called target.  Look for both.
    val dirs = Path.onlyDirs(bases flatMap (x => List(x / "build", x / "target")))

    dirs.headOption getOrElse sys.error("Neither 'build' nor 'target' dir found under test root " + testRoot + ".")
  }

  def srcSpecLib     = findJar("instrumented", Directory(srcDir / "speclib"))
  def srcCodeLib     = findJar("code",  Directory(srcDir / "codelib"), Directory(testRoot / "files" / "codelib") /* work with --srcpath pending */)
  def agentLib       = findJar("scala-partest-javaagent", buildPackLibDir)
  def scalaCheck     = findJar("scalacheck", buildPackLibDir, srcLibDir)
  def testInterface  = findJar("test-interface", buildPackLibDir, srcLibDir)
  def diffUtils      = findJar("diffutils", buildPackLibDir)
}
