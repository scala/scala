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

  def classPathFromTrifecta(library: Path, reflect: Path, compiler: Path) = {
    val usingJars = library.getAbsolutePath endsWith ".jar"
    // basedir for jars or classfiles on core classpath
    val baseDir = SFile(library).parent

    def relativeToLibrary(what: String): Path = {
      if (usingJars) (baseDir / s"$what.jar")
      else (baseDir.parent / "classes" / what)
    }

    // all jars or dirs with prefix `what`
    def relativeToLibraryAll(what: String): Iterator[Path] = (
      if (usingJars) jarsWithPrefix(baseDir, what)
      else dirsWithPrefix(baseDir.parent / "classes" toDirectory, what)
    )

    List[Path](
      library, reflect, compiler,
      relativeToLibrary("scala-actors"),
      relativeToLibrary("scala-parser-combinators"),
      relativeToLibrary("scala-xml"),
      relativeToLibrary("scala-scaladoc"),
      relativeToLibrary("scala-interactive"),
      relativeToLibrary("scalap"),
      PathSettings.diffUtils.fold(sys.error, identity)
    ) ++ relativeToLibraryAll("scala-partest")
  }

  // find library/reflect/compiler jar or subdir under build/$stage/classes/
  // TODO: make more robust -- for now, only matching on prefix of jar file so it works for ivy/maven-resolved versioned jars
  // can we use the ClassLoader to find the jar/directory that contains a characteristic class file?
  def fromClassPath(name: String, testClassPath: List[Path]): Path = testClassPath find (f =>
    (f.extension == "jar" && f.getName.startsWith(s"scala-$name"))
      || (f.absolutePathSegments endsWith Seq("classes", name))
  ) getOrElse sys.error(s"Provided compilationPath does not contain a Scala $name element.\nLooked in: ${testClassPath.mkString(":")}")
}

class FileManager(val testClassPath: List[Path],
                  val libraryUnderTest: Path,
                  val reflectUnderTest: Path,
                  val compilerUnderTest: Path,
                  javaCmd: Option[String] = None,
                  javacCmd: Option[String] = None,
                  scalacOpts: Seq[String] = Seq.empty) {
  def this(testClassPath: List[Path], javaCmd: Option[String] = None, javacCmd: Option[String] = None, scalacOpts: Seq[String] = Seq.empty) {
    this(testClassPath,
        FileManager.fromClassPath("library", testClassPath),
        FileManager.fromClassPath("reflect", testClassPath),
        FileManager.fromClassPath("compiler", testClassPath),
        javaCmd,
        javacCmd,
        scalacOpts)
  }

  def this(libraryUnderTest: Path, reflectUnderTest: Path, compilerUnderTest: Path) {
    this(FileManager.classPathFromTrifecta(libraryUnderTest, reflectUnderTest, compilerUnderTest), libraryUnderTest, reflectUnderTest, compilerUnderTest)
  }

  def this(testClassPath: List[Path], trifecta: (Path, Path, Path)) {
    this(testClassPath, trifecta._1, trifecta._2, trifecta._3)
  }

  lazy val testClassLoader =
    ScalaClassLoader fromURLs (testClassPath map (_.toURI.toURL))

  // basedir for jars or classfiles on core classpath
  lazy val baseDir = libraryUnderTest.parent

  lazy val JAVACMD: String          = javaCmd getOrElse PartestDefaults.javaCmd
  lazy val JAVAC_CMD: String        = javacCmd getOrElse PartestDefaults.javacCmd
  lazy val SCALAC_OPTS: Seq[String] = scalacOpts ++ PartestDefaults.scalacOpts.split(' ').toSeq
  lazy val JAVA_OPTS: String        = PartestDefaults.javaOpts

  def distKind = {
    val p = libraryUnderTest.getAbsolutePath
    if (p endsWith "build/quick/classes/library") "quick"
    else if (p endsWith "build/pack/lib/scala-library.jar") "pack"
    else if (p endsWith "dists/latest/lib/scala-library.jar/") "latest"
    else "installed"
  }

  /** Only when --debug is given. */
  private[this] lazy val testTimings = new mutable.HashMap[String, Long]
  def recordTestTiming(name: String, milliseconds: Long) =
    synchronized { testTimings(name) = milliseconds }


  /** Massage args to merge plugins and fix paths.
   *  Plugin path can be relative to test root, or cwd is out.
   *  While we're at it, mix in the baseline options, too.
   *  That's how ant passes in the plugins dir.
   */
  def updatePluginPath(args: List[String], out: AbstractFile, srcdir: AbstractFile): List[String] = {
    val dir = PathSettings.testRoot
    // The given path, or the output dir if ".", or a temp dir if output is virtual (since plugin loading doesn't like virtual)
    def pathOrCwd(p: String) =
      if (p == ".") {
        val plugxml = "scalac-plugin.xml"
        val pout = if (out.isVirtual) Directory.makeTemp() else Path(out.path)
        val srcpath = Path(srcdir.path)
        val pd = (srcpath / plugxml).toFile
        if (pd.exists) pd copyTo (pout / plugxml)
        pout
      } else Path(p)
    def absolutize(path: String) = pathOrCwd(path) match {
      case x if x.isAbsolute => x.path
      case x                 => (dir / x).toAbsolute.path
    }

    val xprefix = "-Xplugin:"
    val (xplugs, others) = args partition (_ startsWith xprefix)
    val Xplugin = if (xplugs.isEmpty) Nil else List(xprefix +
      (xplugs map (_ stripPrefix xprefix) flatMap (_ split pathSeparator) map absolutize mkString pathSeparator)
    )
    SCALAC_OPTS.toList ::: others ::: Xplugin
  }
}
