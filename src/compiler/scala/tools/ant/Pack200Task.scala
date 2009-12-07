/*                     __                                               *\
**     ________ ___   / /  ___     Scala Ant Tasks                      **
**    / __/ __// _ | / /  / _ |    (c) 2005-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.tools.ant

import java.io.{BufferedOutputStream, File, FileInputStream,
       FileOutputStream, PipedInputStream, PipedOutputStream}
import java.util.jar.{JarFile, JarInputStream, JarOutputStream, Pack200}
import java.util.jar.Pack200.Packer._

import org.apache.tools.ant.{BuildException, DirectoryScanner}
import org.apache.tools.ant.taskdefs.MatchingTask
import org.apache.tools.ant.types.FileSet

/** <p>
 *    An Ant task that applies the pack200 encoding to a JAR file.
 *  </p><ul>
 *  <li>destdir (mandatory),</li>
 *  <li>dir (defaults to project's basedir),</li>
 *  <li>effort (default 9),</li>
 *  <li>keepFileOrder (default false),</li>
 *  <li>keepModificationTime (default false),</li>
 *  <li>repack (default false),</li>
 *  <li>segmentLimit (default -1 for no limit) </li>
 *  <li>suffix (default ".pack")</li>
 *  </ul>
 *
 * @author  James Matlik
 */
class Pack200Task extends MatchingTask {

/*============================================================================*\
**                             Ant user-properties                            **
\*============================================================================*/

  var destdir: Option[File] = None
  var srcdir: Option[File] = None

  var effort = 9
  var keepFileOrder = false
  var keepModificationTime = false
  var repack = false
  var segmentLimit = -1

  var packFileSuffix = ".pack"


/*============================================================================*\
**                             Properties setters                             **
\*============================================================================*/

  def setDir(dir: File) {
    if (dir.exists && dir.isDirectory) srcdir = Some(dir)
    else error("Please specify a valid directory with Jar files for packing.")
  }

  /** A level from 0 (none) to 9 (max) of effort for applying Pack200 */
  def setEffort(x: Int) {
    if (effort < 10 && effort > -1) effort = x
    else error("The effort level must be a value from 0 to 9")
  }

  /** Set the flag to specify if file reordering should be performed. Reordering
    * is used to remove empty packages and improve pack200 optimization.
    * @param keep
    *         true to retain file ordering.
    *         false to optomize directory structure (DEFAULT).  */
  def setKeepFileOrder(x: Boolean) { keepFileOrder = x }

  /** If false, a single modification time is used for all contained files */
  def setKeepModificationTime(x: Boolean) { keepModificationTime = x }

  /** A flag that tells the task to pack and then unpack the source JAR file
    * into another JAR file.  This resulting JAR file can then be signed,
    * packed again, compressed and distributed for securely distributed code.
    */
  def setRepack(r: Boolean) { repack = r }


  def setSegmentLimit(size: Int) { segmentLimit = size }

  /** Set the output directory */
  def setDestdir(file: File) {
    if (file != null && file.exists && file.isDirectory) destdir = Some(file)
    else error("The destination directory is invalid: " + file.getAbsolutePath)
  }

  def setSuffix(s: String) { packFileSuffix = s }

/*============================================================================*\
**                             Properties getters                             **
\*============================================================================*/

  /** Gets the list of individual JAR files for processing.
    * @returns The list of JAR files */
  private def getFileList: List[File] = {
    var files: List[File] = Nil
    val fs = getImplicitFileSet
    var ds = fs.getDirectoryScanner(getProject())
    var dir = fs.getDir(getProject())
    for (filename <- ds.getIncludedFiles()
         if filename.toLowerCase.endsWith(".jar")) {
      val file = new File(dir, filename)
      if(files.exists(file.equals(_)) == false) files = file :: files
    }
    files.reverse
  }

/*============================================================================*\
**                       Compilation and support methods                      **
\*============================================================================*/

/** Generates a build error. Error location will be the current task in the
   * ant file.
   * @param message A message describing the error.
   * @throws BuildException A build error exception thrown in every case. */
  private def error(message: String): Nothing =
    throw new BuildException(message, getLocation())

  private def makeJarOutputStream(file: File) =
    new JarOutputStream(makeOutputStream(file))

  private def makeOutputStream(file: File) =
    new BufferedOutputStream(new FileOutputStream(file))

/*============================================================================*\
**                           The big execute method                           **
\*============================================================================*/

  /** Performs the tool creation. */
  override def execute() = {
    // Audits
    val packDir = destdir.getOrElse(error("No output directory specified"))

    // Setup the inherited fileset for further processing
    fileset.setDir(srcdir.getOrElse(getProject.getBaseDir))

    val files = getFileList
    if (files.isEmpty) error("No JAR files were selected for packing.")

    // Setup the packer
    val packer = Pack200.newPacker
    val p = packer.properties
    p.put(EFFORT, effort.toString)
    p.put(SEGMENT_LIMIT, segmentLimit.toString)
    p.put(KEEP_FILE_ORDER, if(keepFileOrder) TRUE else FALSE)
    p.put(MODIFICATION_TIME, if(keepModificationTime) LATEST else KEEP)

    for (file <- files) {
      if (repack) {
        val repackedFile = new File(packDir, file.getName)
        if (file.lastModified > repackedFile.lastModified) {
          println("Repacking " + file.toString + " to " + repackedFile.toString)
          val tmpFile = new File(packDir, file.getName + ".tmp")
          val os = makeOutputStream(tmpFile)
          packer.pack(new JarFile(file), os)
          os.close()
          val jos = makeJarOutputStream(repackedFile)
          Pack200.newUnpacker.unpack(tmpFile, jos)
          jos.close()
          tmpFile.delete()
        }
      }
      else {
        val packFile: File = {
          val name = file.getName.substring(0, file.getName.lastIndexOf("."))
          new File(packDir, name + packFileSuffix)
        }
        if(file.lastModified > packFile.lastModified) {
          println("Packing " + file.toString + " to " + packFile.toString)
          val os = makeOutputStream(packFile)
          packer.pack(new JarFile(file), os)
        }
      }
    }
  }
}
