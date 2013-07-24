/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest
package nest

import java.io.{
  File,
  FilenameFilter,
  IOException,
  StringWriter,
  FileInputStream,
  FileOutputStream,
  BufferedReader,
  FileReader,
  PrintWriter,
  FileWriter
}
import java.net.URI
import scala.reflect.io.AbstractFile
import scala.collection.mutable
import scala.reflect.internal.util.ScalaClassLoader
import java.net.URLClassLoader

object FileManager {
  def getLogFile(dir: File, fileBase: String, kind: String): File =
    new File(dir, fileBase + "-" + kind + ".log")

  def getLogFile(file: File, kind: String): File = {
    val dir = file.getParentFile
    val fileBase = basename(file.getName)

    getLogFile(dir, fileBase, kind)
  }

  def logFileExists(file: File, kind: String) =
    getLogFile(file, kind).canRead

  def overwriteFileWith(dest: File, file: File) =
    dest.isFile && copyFile(file, dest)

  def copyFile(from: File, dest: File): Boolean = {
    if (from.isDirectory) {
      assert(dest.isDirectory, "cannot copy directory to file")
      val subDir: Directory = Path(dest) / Directory(from.getName)
      subDir.createDirectory()
      from.listFiles.toList forall (copyFile(_, subDir))
    } else {
      val to = if (dest.isDirectory) new File(dest, from.getName) else dest

      try {
        SFile(to) writeAll SFile(from).slurp()
        true
      } catch { case _: IOException => false }
    }
  }

  def mapFile(file: File, replace: String => String) {
    val f = SFile(file)

    f.printlnAll(f.lines.toList map replace: _*)
  }

  def jarsWithPrefix(dir: Directory, name: String): Iterator[SFile] =
    dir.files filter (f => (f hasExtension "jar") && (f.name startsWith name))

  def dirsWithPrefix(dir: Directory, name: String): Iterator[Directory] =
    dir.dirs filter (_.name startsWith name)

  def joinPaths(paths: List[Path]) = ClassPath.join(paths.map(_.getAbsolutePath).distinct: _*)

  /** Compares two files using difflib to produce a unified diff.
   *
   *  @param  original  the first file to be compared
   *  @param  revised  the second file to be compared
   *  @return the unified diff of the compared files or the empty string if they're equal
   */
  def compareFiles(original: File, revised: File): String = {
    compareContents(io.Source.fromFile(original).getLines.toSeq, io.Source.fromFile(revised).getLines.toSeq, original.getName, revised.getName)
  }

  /** Compares two lists of lines using difflib to produce a unified diff.
   *
   *  @param  origLines  the first seq of lines to be compared
   *  @param  newLines   the second seq of lines to be compared
   *  @param  origName   file name to be used in unified diff for `origLines`
   *  @param  newName    file name to be used in unified diff for `newLines`
   *  @return the unified diff of the `origLines` and `newLines` or the empty string if they're equal
   */
  def compareContents(original: Seq[String], revised: Seq[String], originalName: String = "a", revisedName: String = "b"): String = {
    import collection.JavaConverters._

    val diff = difflib.DiffUtils.diff(original.asJava, revised.asJava)
    if (diff.getDeltas.isEmpty) ""
    else difflib.DiffUtils.generateUnifiedDiff(originalName, revisedName, original.asJava, diff, 1).asScala.mkString("\n")
  }
}

class FileManager(val testClassLoader: URLClassLoader) {
  lazy val libraryUnderTest: Path  = findArtifact("library")
  lazy val reflectUnderTest: Path  = findArtifact("reflect")
  lazy val compilerUnderTest: Path = findArtifact("compiler")

  lazy val testClassPath = testClassLoader.getURLs().map(url => Path(new File(url.toURI))).toList

  def this(testClassPath: List[Path]) {
    this(new URLClassLoader(testClassPath.toArray map (_.toURI.toURL)))
  }

  def distKind = {
    val p = libraryUnderTest.getAbsolutePath
    if (p endsWith "build/quick/classes/library") "quick"
    else if (p endsWith "build/pack/lib/scala-library.jar") "pack"
    else if (p endsWith "dists/latest/lib/scala-library.jar") "latest"
    else "installed"
  }

  // find library/reflect/compiler jar or subdir under build/$stage/classes/
  private def findArtifact(name: String): Path = {
    val canaryClass =
      name match {
        case "library"  => Class.forName("scala.Unit", false, testClassLoader)
        case "reflect"  => Class.forName("scala.reflect.api.Symbols", false, testClassLoader)
        case "compiler" => Class.forName("scala.tools.nsc.Main", false, testClassLoader)
      }

    val path = Path(canaryClass.getProtectionDomain.getCodeSource.getLocation.getPath)
    if (path.extension == "jar"
      || path.absolutePathSegments.endsWith(Seq("classes", name))) path
    else sys.error(
      s"""Test Classloader does not contain a $name jar or classes/$name directory.
           |Looked in: $testClassPath""")
  }
}
