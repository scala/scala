package scala.tools.xsbt

import xsbti.compile.{CompileProgress, DependencyChanges}
import xsbti.{BasicVirtualFileRef, Logger, Position, Problem, Severity, VirtualFile, VirtualFileRef, Reporter => XReporter}

import java.io.{ByteArrayInputStream, File, InputStream}
import java.nio.file.{Files, Path}
import java.util.function.Supplier
import scala.collection.mutable.ListBuffer
import scala.tools.nsc.io.AbstractFile
import scala.tools.testkit.TempDir
import scala.util.hashing.MurmurHash3

class BridgeTesting {
  def withTemporaryDirectory[T](action: Path => T): T = {
    val dir = TempDir.createTempDir().toPath
    try action(dir)
    finally AbstractFile.getDirectory(dir.toFile).delete()
  }

  def mkReporter: TestingReporter = new TestingReporter()

  def mkCompiler: CompilerBridge = new CompilerBridge()

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

  def compileSrcs(baseDir: Path, reporter: XReporter, srcs: String*): (List[VirtualFile], TestCallback) = {
    val targetDir = baseDir / "target"
    Files.createDirectory(targetDir)
    val analysisCallback = new TestCallback
    val files = for ((src, i) <- srcs.toList.zipWithIndex) yield new StringVirtualFile(s"Test-$i.scala", src)
    val compiler = mkCompiler
    compiler.run(
      sources = files.toArray,
      changes = emptyChanges,
      options = Array("-usejavacp", "-deprecation"),
      output = new TestOutput(targetDir),
      callback = analysisCallback,
      delegate = reporter,
      progress = ignoreProgress,
      log = TestLogger)
    (files, analysisCallback)
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
