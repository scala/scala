/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 */

package scala.tools
package partest

import util._
import nsc.io._
import Properties._

/** An agglomeration of code which is low on thrills.  Hopefully
 *  it operates so quietly in the background that you never have to
 *  look at this file.
 */
trait Housekeeping {
  self: Universe =>

  /** Orderly shutdown on ctrl-C. */
  private var _shuttingDown = false
  protected def setShuttingDown() = {
    warning("Received shutdown signal, partest is cleaning up...\n")
    _shuttingDown = true
    false
  }
  def isShuttingDown = _shuttingDown

  /** Search for a directory, possibly given only a name, by starting
   *  at the current dir and walking upward looking for it at each level.
   */
  protected def searchForDir(name: String): Directory = {
    val result = Path(name) ifDirectory (x => x.normalize) orElse {
      val cwd = Directory.Current getOrElse error("user.dir property not set")
      val dirs = cwd :: cwd.parents map (_ / name)

      Path onlyDirs dirs map (_.normalize) headOption
    }

    result getOrElse error("Fatal: could not find directory '%s'" format name)
  }

  /** Paths we ignore for most purposes.
   */
  def ignorePath(x: Path) = {
    (x.name startsWith ".") ||
    (x.isDirectory && ((x.name == "lib") || x.hasExtension("obj", "svn")))
  }
  /** Make a possibly relative path absolute using partestDir as the base.
   */
  def absolutize(path: String) = Path(path) toAbsoluteWithRoot partestDir

  /** Go on a deleting binge.
   */
  def cleanupAll() {
    if (isNoCleanup)
      return

    val (dirCount, fileCount) = (cleanupObjDirs(), cleanupLogs() + cleanupJunk())
    if (dirCount + fileCount > 0)
      normal("Cleaned up %d directories and %d files.\n".format(dirCount, fileCount))
  }

  def cleanupObjDirs()  = countTrue(allObjDirs collect { case x if x.exists => x.deleteRecursively() })
  def cleanupJunk()     = countTrue(allClassFiles collect { case x if x.exists => x.delete() })
  def cleanupLogs()     = countTrue(allLogFiles collect { case x if x.exists => x.delete() })

  /** Look through every file in the partest directory and ask around
   *  to make sure someone knows him.  Complain about strangers.
   */
  def validateAll() {
    def denotesTest(p: Path)  = allCategories exists (_ denotesTest p)
    def isMSILcheck(p: Path)  = p.name endsWith "-msil.check"

    def analyzeCategory(cat: DirBasedCategory) = {
      val allTests    = cat.enumerate
      val otherPaths  = cat.root walkFilter (x => !ignorePath(x)) filterNot (cat denotesTest _) filterNot isMSILcheck toList
      val count       = otherPaths.size

      println("Validating %d non-test paths in %s.".format(count, cat.kind))

      for (path <- otherPaths) {
        (allTests find (_ acknowledges path)) match {
          case Some(test)   => if (isVerbose) println("  OK: '%s' is claimed by '%s'".format(path, test.label))
          case _            => println(">> Unknown path '%s'" format path)
        }
      }
    }

    allCategories collect { case x: DirBasedCategory => analyzeCategory(x) }
  }

  trait TestHousekeeping {
    self: TestEntity =>

    /** Calculating derived files.  Given a test like
     *    files/run/foo.scala  or  files/run/foo/
     *  This creates paths like foo.check, foo.flags, etc.
     */
    def withExtension(extension: String) = categoryDir / "%s.%s".format(label, extension)

    /** True for a path if this test acknowledges it belongs to this test.
     *  Overridden by some categories.
     */
    def acknowledges(path: Path): Boolean = {
      val loc = location.normalize
      val knownPaths = List(scalaOptsFile, javaOptsFile, commandFile, logFile, checkFile) ++ jarsInTestDir
      def isContainedSource = location.isDirectory && isJavaOrScala(path) && (path.normalize startsWith loc)

      (knownPaths exists (_ isSame path)) || isContainedSource
    }

    /** This test "responds to" this String.  This could mean anything -- it's a
     *  way of specifying ad-hoc collections of tests to exercise only a subset of tests.
     *  At present it looks for the given String in all the test sources.
     */
    def respondsToString(str: String) = containsString(str)
    def containsString(str: String)   = {
      debug("Checking %s for \"%s\"".format(sourceFiles mkString ", ", str))
      sourceFiles map safeSlurp exists (_ contains str)
    }

    def possiblyTimed[T](body: => T): T = {
      if (isStats) timed(recordTestTiming(label, _))(body)
      else body
    }

    private def prepareForTestRun() = {
      // make sure we have a clean slate
      deleteLog(force = true)
      if (outDir.exists)
        outDir.deleteRecursively()

      // recreate object dir
      outDir createDirectory true
    }
    def deleteOutDir() =
      if (isNoCleanup && outDir.isDirectory) debug("Not deleting " + outDir)
      else outDir.deleteRecursively()

    def cleanup() {
      // otherwise, delete obj dir and logs on success
      deleteOutDir()
      if (isSuccess)
        deleteLog()
    }

    protected def runWrappers[T](body: => T): Option[T] = {
      prepareForTestRun()

      withShutdownHook({ debug("Shutdown hook deleting " + outDir) ; deleteOutDir }) {
        loggingOutAndErr {
          possiblyTimed {
            body
          }
        }
      }
    }

    override def toString = location.path
    override def equals(other: Any) = other match {
      case x: TestEntity  => location.normalize == x.location.normalize
      case _              => false
    }
    override def hashCode = location.normalize.hashCode
  }

  private def countTrue(f: => Iterator[Boolean]) = f filter (_ == true) length
}