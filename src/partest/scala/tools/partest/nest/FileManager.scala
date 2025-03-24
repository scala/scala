/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

// $Id$

package scala.tools.partest
package nest

import java.io.{File, IOException}
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

  def mapFile(file: File, replace: String => String): Unit = {
    val f = SFile(file)

    f.printlnAll(f.lines().toList map replace: _*)
  }

  def jarsWithPrefix(dir: Directory, name: String): Iterator[SFile] =
    dir.files filter (f => (f hasExtension "jar") && (f.name startsWith name))

  def dirsWithPrefix(dir: Directory, name: String): Iterator[Directory] =
    dir.dirs filter (_.name startsWith name)

  def joinPaths(paths: List[Path]) = ClassPath.join(paths.map(_.getAbsolutePath).distinct: _*)

  /** Compares two lists of lines using difflib to produce a unified diff.
   *
   *  @param  origLines  the first seq of lines to be compared
   *  @param  newLines   the second seq of lines to be compared
   *  @param  origName   file name to be used in unified diff for `origLines`
   *  @param  newName    file name to be used in unified diff for `newLines`
   *  @return the unified diff of the `origLines` and `newLines` or the empty string if they're equal
   */
  def compareContents(original: Seq[String], revised: Seq[String], originalName: String = "a", revisedName: String = "b"): String = {
    import scala.jdk.CollectionConverters._
    import com.github.difflib.{DiffUtils, UnifiedDiffUtils}

    val diff = DiffUtils.diff(original.asJava, revised.asJava)
    if (diff.getDeltas.isEmpty) ""
    else UnifiedDiffUtils.generateUnifiedDiff(originalName, revisedName, original.asJava, diff, 1).asScala.mkString("\n")
  }

  def withTempFile[A](outFile: File, fileBase: String, lines: Seq[String])(body: File => A): A = {
    val prefix = s"tmp-$fileBase"
    val suffix = ".check"
    val f = File.createTempFile(prefix, suffix, outFile)
    try {
      import scala.reflect.io.{ File => Feil }
      Feil(f).writeAll(lines map (line => f"$line%n"): _*)
      body(f)
    } finally {
      f.delete()
    }
  }
}

class FileManager(val testClassLoader: URLClassLoader) {
  lazy val libraryUnderTest: Path  = findArtifact("library")
  lazy val reflectUnderTest: Path  = findArtifact("reflect")
  lazy val compilerUnderTest: Path = findArtifact("compiler")
  lazy val agentLib: Path = findArtifact("javaagent")

  lazy val testClassPath = testClassLoader.getURLs().map(url => Path(new File(url.toURI))).toList

  def this(testClassPath: List[Path]) =
    this(new URLClassLoader(testClassPath.toArray map (_.toURI.toURL)))

  def distKind = {
    val p = libraryUnderTest.getAbsolutePath
    if (p endsWith "build/quick/classes/library") "quick"
    else if (p endsWith "build/pack/lib/scala-library.jar") "pack"
    else if (p endsWith "dists/latest/lib/scala-library.jar") "latest"
    else "installed"
  }

  // find library/reflect/compiler/javaagent jar or subdir under build/$stage/classes/
  private def findArtifact(name: String): Path = {
    val canaryClass =
      name match {
        case "library"   => Class.forName("scala.Unit", false, testClassLoader)
        case "reflect"   => Class.forName("scala.reflect.api.Symbols", false, testClassLoader)
        case "compiler"  => Class.forName("scala.tools.nsc.Main", false, testClassLoader)
        case "javaagent" => Class.forName("scala.tools.partest.javaagent.ProfilingAgent", false, testClassLoader)
      }

    val path = Path(canaryClass.getProtectionDomain.getCodeSource.getLocation.getPath)
    if (path.extension == "jar"
      || path.absolutePathSegments.endsWith(Seq("classes", name))) path
    else sys.error(
      s"""Test Classloader does not contain a $name jar or classes/$name directory.
           |Looked in: $testClassPath""")
  }
}
