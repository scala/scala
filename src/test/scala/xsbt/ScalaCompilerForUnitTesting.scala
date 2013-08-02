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
	def compileSrc(src: String): SourceAPI = {
		import java.io.FileWriter
		withTemporaryDirectory { temp =>
			val analysisCallback = new TestCallback
			val classesDir = new File(temp, "classes")
			classesDir.mkdir()
			val compiler = prepareCompiler(classesDir, analysisCallback)
			val run = new compiler.Run
			val srcFile = new File(temp, "Test.scala")
			srcFile.createNewFile()
			val fw = new FileWriter(srcFile)
			fw.write(src)
			fw.close()
			run.compile(List(srcFile.getAbsolutePath()))
			analysisCallback.apis(srcFile)
		}
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
