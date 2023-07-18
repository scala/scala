package scala.tools.xsbt

import xsbti.api.ClassLike
import xsbti.api.DependencyContext._
import xsbti.compile.{CompileProgress, DependencyChanges}
import xsbti.{BasicVirtualFileRef, InteractiveConsoleInterface, Logger, Position, Problem, Severity, VirtualFile, VirtualFileRef, Reporter => XReporter}

import java.io.{ByteArrayInputStream, File, InputStream}
import java.nio.file.{Files, Path}
import java.util.Optional
import java.util.function.Supplier
import scala.collection.mutable.ListBuffer
import scala.tools.nsc.io.AbstractFile
import scala.tools.testkit.TempDir
import scala.tools.xsbt.TestCallback.ExtractedClassDependencies
import scala.util.hashing.MurmurHash3

class BridgeTesting {
  def withTemporaryDirectory[T](action: Path => T): T = {
    val dir = TempDir.createTempDir().toPath
    try action(dir)
    finally AbstractFile.getDirectory(dir.toFile).delete()
  }

  def mkReporter: TestingReporter = new TestingReporter()

  def mkCompiler: CompilerBridge = new CompilerBridge()

  def mkConsole: InteractiveConsoleBridgeFactory = new InteractiveConsoleBridgeFactory()

  def mkScaladoc: ScaladocBridge = new ScaladocBridge()

  private val emptyChanges: DependencyChanges = new DependencyChanges {
    override val modifiedLibraries = new Array[VirtualFileRef](0)
    override val modifiedBinaries = new Array[File](0)
    override val modifiedClasses = new Array[String](0)
    override def isEmpty = true
  }

  private val ignoreProgress = new CompileProgress { }

  def compileSrcs(baseDir: Path, srcs: String*): (Seq[VirtualFile], TestCallback) =
    compileSrcs(baseDir, mkReporter, srcs: _*)

  def compileSrcs(baseDir: Path, reporter: XReporter, srcs: String*): (List[VirtualFile], TestCallback) =
    compileSrcss(baseDir, reporter, List(srcs.toList))

  def compileSrcss(baseDir: Path, reporter: XReporter, srcss: List[List[String]]): (List[VirtualFile], TestCallback) = {
    val targetDir = baseDir / "target"
    Files.createDirectory(targetDir)
    val analysisCallback = new TestCallback
    val filess = for ((sourceGroup, groupId) <- srcss.zipWithIndex) yield {
      val files = sourceGroup.zipWithIndex map {
        case (src, i) => new StringVirtualFile(s"Test-$groupId-$i.scala", src)
      }
      val compiler = mkCompiler
      compiler.run(
        sources = files.toArray,
        changes = emptyChanges,
        options = Array("-usejavacp", "-deprecation", "-cp", targetDir.getAbsolutePath),
        output = new TestOutput(targetDir),
        callback = analysisCallback,
        delegate = reporter,
        progress = ignoreProgress,
        log = TestLogger)
      files
    }
    (filess.flatten, analysisCallback)
  }

  def docSrcs(baseDir: Path, srcs: String*): Unit = {
    val targetDir = baseDir / "target"
    Files.createDirectory(targetDir)
    val files = for ((src, i) <- srcs.toList.zipWithIndex) yield new StringVirtualFile(s"Test-$i.scala", src)
    val scaladoc = mkScaladoc
    scaladoc.run(
      sources = files.toArray,
      args = Array("-usejavacp", "-d", targetDir.getAbsolutePath),
      log = TestLogger,
      delegate = mkReporter)
  }

  def extractDependenciesFromSrcs(srcs: String*): ExtractedClassDependencies = withTemporaryDirectory { tmpDir =>
    val (_, testCallback) = compileSrcs(tmpDir, srcs: _*)

    val memberRefDeps = testCallback.classDependencies.toList collect {
      case (target, src, DependencyByMemberRef) => (src, target)
    }
    val inheritanceDeps = testCallback.classDependencies.toList collect {
      case (target, src, DependencyByInheritance) => (src, target)
    }
    val localInheritanceDeps = testCallback.classDependencies.toList collect {
      case (target, src, LocalDependencyByInheritance) => (src, target)
    }
    ExtractedClassDependencies.fromPairs(memberRefDeps, inheritanceDeps, localInheritanceDeps)
  }

  def extractBinaryDependenciesFromSrcss(srcss: List[List[String]]): ExtractedClassDependencies = withTemporaryDirectory { tmpDir =>
    val (_, testCallback) = compileSrcss(tmpDir, mkReporter, srcss)
    val binaryDependencies = testCallback.binaryDependencies
    ExtractedClassDependencies.fromPairs(
      binaryDependencies.toList.collect { case (_, bin, src, DependencyByMemberRef) => src -> bin },
      binaryDependencies.toList.collect { case (_, bin, src, DependencyByInheritance) =>
        src -> bin
      },
      binaryDependencies.toList.collect { case (_, bin, src, LocalDependencyByInheritance) =>
        src -> bin
      },
    )
  }

  /**
   * Compiles given source code using Scala compiler and returns API representation
   * extracted by ExtractAPI class.
   */
  def extractApisFromSrc(src: String): Set[ClassLike] = withTemporaryDirectory { tmpDir =>
    val (Seq(tempSrcFile), analysisCallback) = compileSrcs(tmpDir, src)
    analysisCallback.apis(tempSrcFile)
  }

  /**
   * Compiles given source code using Scala compiler and returns API representation
   * extracted by ExtractAPI class.
   */
  def extractApisFromSrcs(srcs: List[String]*): Seq[Set[ClassLike]] = withTemporaryDirectory { tmpDir =>
    val (tempSrcFiles, analysisCallback) = compileSrcss(tmpDir, mkReporter, srcs.toList)
    tempSrcFiles.map(analysisCallback.apis)
  }

  def extractUsedNamesFromSrc(src: String): Map[String, Set[String]] = withTemporaryDirectory { tmpDir =>
    val (_, analysisCallback) = compileSrcs(tmpDir, src)
    analysisCallback.usedNames.toMap
  }

  /**
   * Extract used names from the last source file in `sources`.
   *
   * The previous source files are provided to successfully compile examples.
   * Only the names used in the last src file are returned.
   */
  def extractUsedNamesFromSrc(sources: String*): Map[String, Set[String]] = withTemporaryDirectory { tmpDir =>
    val (srcFiles, analysisCallback) = compileSrcs(tmpDir, sources: _*)
    srcFiles
      .map { srcFile =>
        val classesInSrc = analysisCallback.classNames(srcFile).map(_._1)
        classesInSrc.map(className => className -> analysisCallback.usedNames(className)).toMap
      }
      .reduce(_ ++ _)
  }

  def interactiveConsole(baseDir: Path)(args: String*): InteractiveConsoleInterface = {
    val targetDir = baseDir / "target"
    Files.createDirectory(targetDir)
    val sc = mkConsole
    sc.createConsole(
      args = Array("-usejavacp") ++ args,
      bootClasspathString = "",
      classpathString = "",
      initialCommands = "",
      cleanupCommands = "",
      loader = Optional.empty,
      bindNames = Array(),
      bindValues = Array(),
      log = TestLogger)
  }

  def withInteractiveConsole[A](f: InteractiveConsoleInterface => A): A = withTemporaryDirectory { tmpDir =>
    val repl = interactiveConsole(tmpDir)()
    try f(repl)
    finally repl.close()
  }
}

class TestingReporter extends XReporter {
  val messages: ListBuffer[Problem] = ListBuffer.empty

  override def reset(): Unit = messages.clear()
  override def hasErrors: Boolean = messages.exists(_.severity() == Severity.Error)
  override def hasWarnings: Boolean = messages.exists(_.severity() == Severity.Warn)
  override def printSummary(): Unit = println(messages.mkString("\n"))
  override def problems(): Array[Problem] = messages.toArray
  override def log(problem: Problem): Unit = messages += problem
  override def comment(pos: Position, msg: String): Unit = ()
}

class StringVirtualFile(path: String, content: String) extends BasicVirtualFileRef(path) with VirtualFile {
  override def contentHash: Long = MurmurHash3.arrayHash(content.getBytes("UTF-8"))
  override def input: InputStream = new ByteArrayInputStream(content.getBytes("UTF-8"))
  override def toString: String = s"StringVirtualFile($path, <content>)"
}

class TestOutput(override val getOutputDirectoryAsPath: Path) extends xsbti.compile.SingleOutput {
  override def getOutputDirectory: File = getOutputDirectoryAsPath.toFile
  override def toString: String = s"TestOutput($getOutputDirectoryAsPath)"
}

object TestLogger extends Logger {
  override def error(msg: Supplier[String]): Unit = throw new Exception(msg.get())
  override def warn(msg: Supplier[String]): Unit = println(msg.get())
  override def info(msg: Supplier[String]): Unit = println(msg.get())
  override def debug(msg: Supplier[String]): Unit = ()
  override def trace(exception: Supplier[Throwable]): Unit = ()
}
