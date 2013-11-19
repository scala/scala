package xsbt

import xsbti.compile.SingleOutput
import java.io.File
import _root_.scala.tools.nsc.reporters.ConsoleReporter
import _root_.scala.tools.nsc.Settings
import xsbti._
import xsbti.api.SourceAPI
import sbt.IO.withTemporaryDirectory
import xsbti.api.ClassLike
import xsbti.api.Definition
import xsbti.api.Def
import xsbt.api.SameAPI
import sbt.ConsoleLogger

/**
 * Provides common functionality needed for unit tests that require compiling
 * source code using Scala compiler.
 */
class ScalaCompilerForUnitTesting {

	/**
	 * Compiles given source code using Scala compiler and returns API representation
	 * extracted by ExtractAPI class.
	 */
	def extractApiFromSrc(src: String): SourceAPI = {
		val (Seq(tempSrcFile), analysisCallback) = compileSrcs(src)
		analysisCallback.apis(tempSrcFile)
	}

	/**
	 * Compiles given source code snippets written to a temporary files. Each snippet is
	 * written to a separate temporary file.
	 *
	 * The sequence of temporary files corresponding to passed snippets and analysis
	 * callback is returned as a result.
	 */
	private def compileSrcs(srcs: String*): (Seq[File], TestCallback) = {
		withTemporaryDirectory { temp =>
			val analysisCallback = new TestCallback
			val classesDir = new File(temp, "classes")
			classesDir.mkdir()
			val compiler = prepareCompiler(classesDir, analysisCallback)
			val run = new compiler.Run
			val srcFiles = srcs.toSeq.zipWithIndex map { case (src, i) =>
				val fileName = s"Test_$i.scala"
				prepareSrcFile(temp, fileName, src)
			}
			val srcFilePaths = srcFiles.map(srcFile => srcFile.getAbsolutePath).toList
			run.compile(srcFilePaths)
			(srcFiles, analysisCallback)
		}
	}

	private def prepareSrcFile(baseDir: File, fileName: String, src: String): File = {
		import java.io.FileWriter
		val srcFile = new File(baseDir, fileName)
		srcFile.createNewFile()
		val fw = new FileWriter(srcFile)
		fw.write(src)
		fw.close()
		srcFile
	}

	private def prepareCompiler(outputDir: File, analysisCallback: AnalysisCallback): CachedCompiler0#Compiler = {
		val args = Array.empty[String]
		object output extends SingleOutput {
			def outputDirectory: File = outputDir
		}
		val weakLog = new WeakLog(ConsoleLogger(), ConsoleReporter)
		val cachedCompiler = new CachedCompiler0(args, output, weakLog, false)
		val settings = cachedCompiler.settings
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
