package xsbt

import xsbti.TestCallback.ExtractedClassDependencies
import xsbti.compile.SingleOutput
import java.io.File
import _root_.scala.tools.nsc.reporters.ConsoleReporter
import xsbti._
import sbt.io.IO.withTemporaryDirectory
import xsbti.api.ClassLike

import sbt.internal.util.ConsoleLogger
import xsbti.api.DependencyContext._

/**
 * Provides common functionality needed for unit tests that require compiling
 * source code using Scala compiler.
 */
class ScalaCompilerForUnitTesting {

  /**
   * Compiles given source code using Scala compiler and returns API representation
   * extracted by ExtractAPI class.
   */
  def extractApisFromSrc(src: String): Set[ClassLike] = {
    val (Seq(tempSrcFile), analysisCallback) = compileSrcs(src)
    analysisCallback.apis(tempSrcFile)
  }

  /**
   * Compiles given source code using Scala compiler and returns API representation
   * extracted by ExtractAPI class.
   */
  def extractApisFromSrcs(reuseCompilerInstance: Boolean)(srcs: List[String]*): Seq[Set[ClassLike]] = {
    val (tempSrcFiles, analysisCallback) = compileSrcs(srcs.toList, reuseCompilerInstance)
    tempSrcFiles.map(analysisCallback.apis)
  }

  def extractUsedNamesFromSrc(src: String): Map[String, Set[String]] = {
    val (_, analysisCallback) = compileSrcs(src)
    analysisCallback.usedNames.toMap
  }

  def extractBinaryClassNamesFromSrc(src: String): Set[(String, String)] = {
    val (Seq(tempSrcFile), analysisCallback) = compileSrcs(src)
    analysisCallback.classNames(tempSrcFile).toSet
  }

  /**
   * Extract used names from src provided as the second argument.
   *
   * The purpose of the first argument is to define names that the second
   * source is going to refer to. Both files are compiled in the same compiler
   * Run but only names used in the second src file are returned.
   */
  def extractUsedNamesFromSrc(definitionSrc: String, actualSrc: String): Map[String, Set[String]] = {
    // we drop temp src file corresponding to the definition src file
    val (Seq(_, tempSrcFile), analysisCallback) = compileSrcs(definitionSrc, actualSrc)
    val classesInActualSrc = analysisCallback.classNames(tempSrcFile).map(_._1)
    classesInActualSrc.map(className => className -> analysisCallback.usedNames(className)).toMap
  }

  /**
   * Extract used names from the last source file in `sources`.
   *
   * The previous source files are provided to successfully compile examples.
   * Only the names used in the last src file are returned.
   */
  def extractUsedNamesFromSrc(sources: String*): Map[String, Set[String]] = {
    val (srcFiles, analysisCallback) = compileSrcs(sources: _*)
    srcFiles.map { srcFile =>
      val classesInSrc = analysisCallback.classNames(srcFile).map(_._1)
      classesInSrc.map(className => className -> analysisCallback.usedNames(className)).toMap
    }.reduce(_ ++ _)
  }

  /**
   * Compiles given source code snippets (passed as Strings) using Scala compiler and returns extracted
   * dependencies between snippets. Source code snippets are identified by symbols. Each symbol should
   * be associated with one snippet only.
   *
   * Snippets can be grouped to be compiled together in the same compiler run. This is
   * useful to compile macros, which cannot be used in the same compilation run that
   * defines them.
   *
   * Symbols are used to express extracted dependencies between source code snippets. This way we have
   * file system-independent way of testing dependencies between source code "files".
   */
  def extractDependenciesFromSrcs(srcs: List[List[String]]): ExtractedClassDependencies = {
    val (_, testCallback) = compileSrcs(srcs, reuseCompilerInstance = true)

    val memberRefDeps = testCallback.classDependencies collect {
      case (target, src, DependencyByMemberRef) => (src, target)
    }
    val inheritanceDeps = testCallback.classDependencies collect {
      case (target, src, DependencyByInheritance) => (src, target)
    }
    val localInheritanceDeps = testCallback.classDependencies collect {
      case (target, src, LocalDependencyByInheritance) => (src, target)
    }
    ExtractedClassDependencies.fromPairs(memberRefDeps, inheritanceDeps, localInheritanceDeps)
  }

  def extractDependenciesFromSrcs(srcs: String*): ExtractedClassDependencies = {
    extractDependenciesFromSrcs(List(srcs.toList))
  }

  /**
   * Compiles given source code snippets written to temporary files. Each snippet is
   * written to a separate temporary file.
   *
   * Snippets can be grouped to be compiled together in the same compiler run. This is
   * useful to compile macros, which cannot be used in the same compilation run that
   * defines them.
   *
   * The `reuseCompilerInstance` parameter controls whether the same Scala compiler instance
   * is reused between compiling source groups. Separate compiler instances can be used to
   * test stability of API representation (with respect to pickling) or to test handling of
   * binary dependencies.
   *
   * The sequence of temporary files corresponding to passed snippets and analysis
   * callback is returned as a result.
   */
  private[xsbt] def compileSrcs(
    groupedSrcs: List[List[String]],
    reuseCompilerInstance: Boolean
  ): (Seq[File], TestCallback) = {
    withTemporaryDirectory { temp =>
      val analysisCallback = new TestCallback
      val classesDir = new File(temp, "classes")
      classesDir.mkdir()

      lazy val commonCompilerInstance = prepareCompiler(classesDir, analysisCallback, classesDir.toString)

      val files = for ((compilationUnit, unitId) <- groupedSrcs.zipWithIndex) yield {
        // use a separate instance of the compiler for each group of sources to
        // have an ability to test for bugs in instability between source and pickled
        // representation of types
        val compiler = if (reuseCompilerInstance) commonCompilerInstance else
          prepareCompiler(classesDir, analysisCallback, classesDir.toString)
        val run = new compiler.Run
        val srcFiles = compilationUnit.toSeq.zipWithIndex map {
          case (src, i) =>
            val fileName = s"Test-$unitId-$i.scala"
            prepareSrcFile(temp, fileName, src)
        }
        val srcFilePaths = srcFiles.map(srcFile => srcFile.getAbsolutePath).toList

        run.compile(srcFilePaths)

        srcFilePaths.foreach(f => new File(f).delete)
        srcFiles
      }
      (files.flatten.toSeq, analysisCallback)
    }
  }

  private def compileSrcs(srcs: String*): (Seq[File], TestCallback) = {
    compileSrcs(List(srcs.toList), reuseCompilerInstance = true)
  }

  private def prepareSrcFile(baseDir: File, fileName: String, src: String): File = {
    val srcFile = new File(baseDir, fileName)
    sbt.io.IO.write(srcFile, src)
    srcFile
  }

  private[xsbt] def prepareCompiler(outputDir: File, analysisCallback: AnalysisCallback, classpath: String = "."): CachedCompiler0#Compiler = {
    val args = Array.empty[String]
    object output extends SingleOutput {
      def outputDirectory: File = outputDir
      override def toString = s"SingleOutput($outputDirectory)"
    }
    val weakLog = new WeakLog(ConsoleLogger(), ConsoleReporter)
    val cachedCompiler = new CachedCompiler0(args, output, weakLog, false)
    val settings = cachedCompiler.settings
    settings.classpath.value = classpath
    settings.usejavacp.value = true
    val scalaReporter = new ConsoleReporter(settings)
    val delegatingReporter = DelegatingReporter(settings, ConsoleReporter)
    val compiler = cachedCompiler.compiler
    compiler.set(analysisCallback, delegatingReporter)
    compiler
  }

  private object ConsoleReporter extends Reporter {
    def reset(): Unit = ()
    def hasErrors: Boolean = false
    def hasWarnings: Boolean = false
    def printWarnings(): Unit = ()
    def problems: Array[Problem] = Array.empty
    def log(pos: Position, msg: String, sev: Severity): Unit = println(msg)
    def comment(pos: Position, msg: String): Unit = ()
    def printSummary(): Unit = ()
  }

}

