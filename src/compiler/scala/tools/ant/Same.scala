/*                     __                                               *\
**     ________ ___   / /  ___     Scala Ant Tasks                      **
**    / __/ __// _ | / /  / _ |    (c) 2005-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package tools.ant

import java.io.{File, FileInputStream}

import org.apache.tools.ant.{BuildException, Project}
import org.apache.tools.ant.util.{FileNameMapper, IdentityMapper}

import org.apache.tools.ant.types.Mapper

/** An Ant task that, for a set of files, tests them for byte-to-byte
 *  equality with one or more other files.
 *
 *  This task supports the following parameters as attributes:
 *  - `dir`
 *  - `todir`
 *  - `resultproperty` (a property to be set when all tested files pairs are
 *    equal, if not set, the task will fail instead),
 *  - `failing` (whether to stop if all files are not equal).
 *
 *  It also support the following nested elements:
 *  - `mapper` (a mapper from original files to test files).
 *
 *  This task itself defines a fileset that represents the set of original files.
 *
 * @author  Gilles Dubochet
 * @version 1.0 */
@deprecated("Use diff", "2.11.0") class Same extends ScalaMatchingTask {
/*============================================================================*\
**                             Ant user-properties                            **
\*============================================================================*/

  private var origin: Option[File] = None
  private var destination: Option[File] = None

  private var resultProperty: Option[String] = None
  private var failing: Boolean = false

  private var mapperElement: Option[Mapper] = None

/*============================================================================*\
**                             Properties setters                             **
\*============================================================================*/

  def setDir(input: File) =
    origin = Some(input)

  def setTodir(input: File) =
    destination = Some(input)

  def setResultproperty(input: String) =
    resultProperty = Some(input)

  def setFailondifferent(input: Boolean) =
    failing = input

  def createMapper(): Mapper =
    if (mapperElement.isEmpty) {
      val mapper = new Mapper(getProject)
      mapperElement = Some(mapper)
      mapper
    }
    else throw new BuildException("Cannot define more than one mapper", getLocation)

  def add(fileNameMapper: FileNameMapper) =
    createMapper().add(fileNameMapper)

/*============================================================================*\
**                             Properties getters                             **
\*============================================================================*/

  private def getMapper: FileNameMapper = mapperElement match {
    case None =>
      new IdentityMapper()
    case Some(me) =>
      me.getImplementation
  }

/*============================================================================*\
**                               Support methods                              **
\*============================================================================*/

  private var allEqualNow = true

  /** Tests if all mandatory attributes are set and valid. */
  private def validateAttributes() = {
    if (origin.isEmpty) sys.error("Mandatory attribute 'dir' is not set.")
    if (destination.isEmpty) sys.error("Mandatory attribute 'todir' is not set.")
  }

  private def reportDiff(f1: File, f2: File) = {
    allEqualNow = false
    log("File '" + f1 + "' is different from correspondant.")
  }

  private def reportMissing(f1: File) = {
    allEqualNow = false
    log("File '" + f1 + "' has no correspondant.")
  }

/*============================================================================*\
**                           The big execute method                           **
\*============================================================================*/

  override def execute() = {
    validateAttributes()
    val mapper = getMapper
    allEqualNow = true
    val originNames: Array[String] = getDirectoryScanner(origin.get).getIncludedFiles
    val bufferSize = 1024
    val originBuffer = new Array[Byte](bufferSize)
    val destBuffer = new Array[Byte](bufferSize)
    for (
      originName: String <- originNames;
      destName: String <- mapper.mapFileName(originName)
    ) {
      //println("originName="+originName)
      //println("destName  ="+destName)
      var equalNow = true
      val originFile = new File(origin.get, originName)
      val destFile = new File(destination.get, destName)
      if (originFile.canRead && destFile.canRead) {
        val originStream = new FileInputStream(originFile)
        val destStream = new FileInputStream(destFile)
        var originRemaining = originStream.read(originBuffer)
        var destRemaining = destStream.read(destBuffer)
        while (originRemaining > 0 && equalNow) {
          if (originRemaining == destRemaining)
            for (idx <- 0 until originRemaining)
              equalNow = equalNow && (originBuffer(idx) == destBuffer(idx))
          else
            equalNow = false
          originRemaining = originStream.read(originBuffer)
          destRemaining = destStream.read(destBuffer)
        }
        if (destRemaining > 0)
          equalNow = false
        if (!equalNow)
          reportDiff(originFile, destFile)
        originStream.close
        destStream.close
      }
      else reportMissing(originFile)
    }
    if (!allEqualNow)
      if (failing)
        sys.error("There were differences between '" + origin.get + "' and '" + destination.get + "'")
      else
        log("There were differences between '" + origin.get + "' and '" + destination.get + "'")
    else {
      if (!resultProperty.isEmpty)
        getProject.setProperty(resultProperty.get, "yes")
      log("All files in '" + origin.get + "' and '" + destination.get + "' are equal", Project.MSG_VERBOSE)
    }
  }

}
