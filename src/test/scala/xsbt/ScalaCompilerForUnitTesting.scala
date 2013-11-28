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

import ScalaCompilerForUnitTesting.ExtractedSourceDependencies

/**
 * Provides common functionality needed for unit tests that require compiling
 * source code using Scala compiler.
 */
class ScalaCompilerForUnitTesting(nameHashing: Boolean = false) {

	/**
	 * Compiles given source code using Scala compiler and returns API representation
	 * extracted by ExtractAPI class.
	 */
	def extractApiFromSrc(src: String): SourceAPI = {
		val (Seq(tempSrcFile), analysisCallback) = compileSrcs(src)
		analysisCallback.apis(tempSrcFile)
	}

	/**
	 * Compiles given source code snippets (passed as Strings) using Scala compiler and returns extracted
	 * dependencies between snippets. Source code snippets are identified by symbols. Each symbol should
	 * be associated with one snippet only.
	 *
	 * Symbols are used to express extracted dependencies between source code snippets. This way we have
	 * file system-independent way of testing dependencies between source code "files".
	 */
	def extractDependenciesFromSrcs(srcs: (Symbol, String)*): ExtractedSourceDependencies = {
		val (symbolsForSrcs, rawSrcs) = srcs.unzip
		assert(symbolsForSrcs.distinct.size == symbolsForSrcs.size,
			s"Duplicate symbols for srcs detected: $symbolsForSrcs")
		val (tempSrcFiles, testCallback) = compileSrcs(rawSrcs: _*)
		val fileToSymbol = (tempSrcFiles zip symbolsForSrcs).toMap
		val memberRefFileDeps =  testCallback.sourceDependencies collect {
			// false indicates that those dependencies are not introduced by inheritance
			case (target, src, false) => (src, target)
		}
		val inheritanceFileDeps =  testCallback.sourceDependencies collect {
			// true indicates that those dependencies are introduced by inheritance
			case (target, src, true) => (src, target)
		}
		def toSymbols(src: File, target: File): (Symbol, Symbol) = (fileToSymbol(src), fileToSymbol(target))
		val memberRefDeps = memberRefFileDeps map { case (src, target) => toSymbols(src, target) }
		val inheritanceDeps = inheritanceFileDeps map { case (src, target) => toSymbols(src, target) }
		def pairsToMultiMap[A, B](pairs: Seq[(A, B)]): Map[A, Set[B]] = {
			import scala.collection.mutable.{HashMap, MultiMap}
			val emptyMultiMap = new HashMap[A, scala.collection.mutable.Set[B]] with MultiMap[A, B]
			val multiMap = pairs.foldLeft(emptyMultiMap) { case (acc, (key, value)) =>
				acc.addBinding(key, value)
			}
			// convert all collections to immutable variants
			multiMap.toMap.mapValues(_.toSet).withDefaultValue(Set.empty)
		}
		ExtractedSourceDependencies(pairsToMultiMap(memberRefDeps), pairsToMultiMap(inheritanceDeps))
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
			val analysisCallback = new TestCallback(nameHashing)
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
		val srcFile = new File(baseDir, fileName)
		sbt.IO.write(srcFile, src)
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

object ScalaCompilerForUnitTesting {
	case class ExtractedSourceDependencies(memberRef: Map[Symbol, Set[Symbol]], inheritance: Map[Symbol, Set[Symbol]])
}
