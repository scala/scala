/*                     __                                               *\
**     ________ ___   / /  ___     Scala Ant Tasks                      **
**    / __/ __// _ | / /  / _ |    (c) 2005-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools.ant

import java.io.{BufferedOutputStream, File, FileOutputStream}
import java.util.jar.{JarFile, JarOutputStream, Pack200}
import java.util.jar.Pack200.Packer._

/** An [[http://ant.apache.org Ant]] task that applies the pack200 encoding
 *  to a JAR file.
 *
 *  - `destdir` (mandatory),
 *  - `dir` (defaults to project's basedir),
 *  - `effort` (default 9),
 *  - `keepFileOrder` (default `'''false'''`),
 *  - `keepModificationTime` (default `'''false'''`),
 *  - `repack` (default false),
 *  - `segmentLimit` (default `-1` for no limit),
 *  - `suffix` (default ".pack")
 *
 * @author  James Matlik
 */
class Pack200Task extends ScalaMatchingTask {

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
    else buildError("Please specify a valid directory with Jar files for packing.")
  }

  /** A level from 0 (none) to 9 (max) of effort for applying Pack200 */
  def setEffort(x: Int) {
    if (effort < 10 && effort > -1) effort = x
    else buildError("The effort level must be a value from 0 to 9")
  }

  /** Set the flag to specify if file reordering should be performed. Reordering
    * is used to remove empty packages and improve pack200 optimization.
    * @param x
    *         `'''true'''` to retain file ordering.
    *         `'''false'''` to optimize directory structure (DEFAULT).  */
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
    else buildError("The destination directory is invalid: " + file.getAbsolutePath)
  }

  def setSuffix(s: String) { packFileSuffix = s }

/*============================================================================*\
**                             Properties getters                             **
\*============================================================================*/

  /** Gets the list of individual JAR files for processing.
    * @return The list of JAR files */
  private def getFileList: List[File] = {
    var files: List[File] = Nil
    val fs = getImplicitFileSet
    val ds = fs.getDirectoryScanner(getProject())
    val dir = fs.getDir(getProject())
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
    val packDir = destdir.getOrElse(buildError("No output directory specified"))

    // Setup the inherited fileset for further processing
    fileset.setDir(srcdir.getOrElse(getProject.getBaseDir))

    val files = getFileList
    if (files.isEmpty) buildError("No JAR files were selected for packing.")

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
