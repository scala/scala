/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import xsbti.{AnalysisCallback,Logger,Problem,Reporter,Severity}
import xsbti.compile.{CachedCompiler, DependencyChanges}
import scala.tools.nsc.{io, reporters, util, Phase, Global, Settings, SubComponent}
import util.{ClassPath,DirectoryClassPath,MergedClassPath,JavaClassPath}
import ClassPath.{ClassPathContext,JavaContext}
import io.AbstractFile
import scala.annotation.tailrec
import scala.collection.mutable
import Log.debug
import java.io.File

final class CompilerInterface
{
	def newCompiler(options: Array[String], initialLog: Logger, initialDelegate: Reporter): CachedCompiler =
		new CachedCompiler0(options, new WeakLog(initialLog, initialDelegate))
	def run(sources: Array[File], changes: DependencyChanges, callback: AnalysisCallback, log: Logger, delegate: Reporter, cached: CachedCompiler): Unit =
		cached.run(sources, changes, callback, log, delegate)
}
sealed abstract class CallbackGlobal(settings: Settings, reporter: reporters.Reporter) extends Global(settings, reporter) {
	def callback: AnalysisCallback
	def findClass(name: String): Option[AbstractFile]
}
class InterfaceCompileFailed(val arguments: Array[String], val problems: Array[Problem], override val toString: String) extends xsbti.CompileFailed

private final class WeakLog(private[this] var log: Logger, private[this] var delegate: Reporter)
{
	def apply(message: String) {
		assert(log ne null, "Stale reference to logger")
		log.error(Message(message))
	}
	def logger: Logger = log
	def reporter: Reporter = delegate
	def clear() {
		log = null
		delegate = null
	}
}

private final class CachedCompiler0(args: Array[String], initialLog: WeakLog) extends CachedCompiler
{
	val settings = new Settings(s => initialLog(s))
	val command = Command(args.toList, settings)
	private[this] val dreporter = DelegatingReporter(settings, initialLog.reporter)
	try {
		compiler // force compiler internal structures
		if(!noErrors(dreporter)) {
			dreporter.printSummary()
			handleErrors(dreporter, initialLog.logger)
		}
	} finally
		initialLog.clear()

	def noErrors(dreporter: DelegatingReporter) = !dreporter.hasErrors && command.ok

	def run(sources: Array[File], changes: DependencyChanges, callback: AnalysisCallback, log: Logger, delegate: Reporter): Unit = synchronized
	{
		println("Running cached compiler " + hashCode.toHexString)
		debug(log, "Interfacing (CompilerInterface) with Scala compiler " + scala.tools.nsc.Properties.versionString)
		val dreporter = DelegatingReporter(settings, delegate)
		try { run(sources.toList, changes, callback, log, dreporter) }
		finally { dreporter.dropDelegate() }
	}
	private[this] def run(sources: List[File], changes: DependencyChanges, callback: AnalysisCallback, log: Logger, dreporter: DelegatingReporter)
	{
		if(command.shouldStopWithInfo)
		{
			dreporter.info(null, command.getInfoMessage(compiler), true)
			throw new InterfaceCompileFailed(args, Array(), "Compiler option supplied that disabled actual compilation.")
		}
		if(noErrors(dreporter))
		{
			debug(log, args.mkString("Calling Scala compiler with arguments  (CompilerInterface):\n\t", "\n\t", ""))
			compiler.set(callback, dreporter)
			try {
				val run = new compiler.Run
				compiler.reload(changes)
				val sortedSourceFiles = sources.map(_.getAbsolutePath).sortWith(_ < _)
				run compile sortedSourceFiles
				processUnreportedWarnings(run)
			} finally {
				compiler.clear()
			}
			dreporter.problems foreach { p => callback.problem(p.position, p.message, p.severity, true) }
		}
		dreporter.printSummary()
		if(!noErrors(dreporter)) handleErrors(dreporter, log)
	}
	def handleErrors(dreporter: DelegatingReporter, log: Logger): Nothing =
	{
		debug(log, "Compilation failed (CompilerInterface)")
		throw new InterfaceCompileFailed(args, dreporter.problems, "Compilation failed")
	}
	def processUnreportedWarnings(run: compiler.Run)
	{
			implicit def listToBoolean[T](l: List[T]): Boolean = error("source compatibility only, should never be called")
			implicit def listToInt[T](l: List[T]): Int = error("source compatibility only, should never be called")
		compiler.logUnreportedWarnings(run.deprecationWarnings)
		compiler.logUnreportedWarnings(run.uncheckedWarnings)
	}
	object compiler extends CallbackGlobal(command.settings, dreporter)
	{
		object dummy // temporary fix for #4426
		object sbtAnalyzer extends
		{
			val global: compiler.type = compiler
			val phaseName = Analyzer.name
			val runsAfter = List("jvm")
			override val runsBefore = List("terminal")
			val runsRightAfter = None
		}
		with SubComponent
		{
			val analyzer = new Analyzer(global)
			def newPhase(prev: Phase) = analyzer.newPhase(prev)
			def name = phaseName
		}
		object apiExtractor extends
		{
			val global: compiler.type = compiler
			val phaseName = API.name
			val runsAfter = List("typer")
			override val runsBefore = List("erasure")
			val runsRightAfter = Some("typer")
		}
		with SubComponent
		{
			val api = new API(global)
			def newPhase(prev: Phase) = api.newPhase(prev)
			def name = phaseName
		}

		val out = new File(settings.outdir.value)
		override lazy val phaseDescriptors =
		{
			phasesSet += sbtAnalyzer
			phasesSet += apiExtractor
			superComputePhaseDescriptors
		}
		// Required because computePhaseDescriptors is private in 2.8 (changed to protected sometime later).
		private[this] def superComputePhaseDescriptors() = superCall("computePhaseDescriptors").asInstanceOf[List[SubComponent]]
		private[this] def superDropRun(): Unit = superCall("dropRun")
		private[this] def superCall(methodName: String): AnyRef =
		{
			val meth = classOf[Global].getDeclaredMethod(methodName)
			meth.setAccessible(true)
			meth.invoke(this)
		}
		def logUnreportedWarnings(seq: List[(Position,String)]): Unit = // Scala 2.10.x and later
		{
			for( (pos, msg) <- seq) yield
				callback.problem(reporter.asInstanceOf[DelegatingReporter].convert(pos), msg, Severity.Warn, false)
		}
		def logUnreportedWarnings(count: Boolean): Unit = () // for source compatibility with Scala 2.8.x
		def logUnreportedWarnings(count: Int): Unit = () // for source compatibility with Scala 2.9.x
		
		def set(callback: AnalysisCallback, dreporter: DelegatingReporter)
		{
			this.callback0 = callback
			reporter = dreporter
		}
		def clear()
		{
			callback0 = null
			atPhase(currentRun.namerPhase) { forgetAll() }
			superDropRun()
			reporter = null
		}

		override def registerTopLevelSym(sym: Symbol) = toForget += sym

		def findClass(name: String): Option[AbstractFile] =
			getOutputClass(name) orElse findOnClassPath(name)

		def getOutputClass(name: String): Option[AbstractFile] =
		{
			val f = new File(out, name.replace('.', '/') + ".class")
			if(f.exists) Some(AbstractFile.getFile(f)) else None
		}

		def findOnClassPath(name: String): Option[AbstractFile] = classPath.findClass(name).flatMap(_.binary.asInstanceOf[Option[AbstractFile]])

		final def unlinkAll(m: Symbol) {
			val scope = m.owner.info.decls
			scope unlink m
			scope unlink m.companionSymbol
//			if(scope.isEmpty && m.owner != definitions.EmptyPackageClass && m.owner != definitions.RootClass)
//				emptyPackages += m.owner
		}
		def reloadClass(pkg: Symbol, simpleName: String, bin: AbstractFile)
		{
			val loader = new loaders.ClassfileLoader(bin)
			toForget += loaders.enterClass(pkg, simpleName, loader)
			toForget += loaders.enterModule(pkg, simpleName, loader)
		}

		def forgetAll()
		{
			for(sym <- toForget) {
				unlinkAll(sym)
				toReload.put(sym.fullName, (sym.owner, sym.name.toString))
			}
			toForget = mutable.Set()
		}

		// fine-control over external changes is unimplemented:
		//   must drop whole CachedCompiler when !changes.isEmpty
		def reload(changes: DependencyChanges)
		{
			for {
				(fullName,(pkg,simpleName)) <- toReload
				classFile <- getOutputClass(fullName)
			}
				reloadClass(pkg, simpleName, classFile)

			toReload = newReloadMap()
		}
		
		private [this] def newReloadMap() = mutable.Map[String,(Symbol,String)]()
		private[this] var emptyPackages = mutable.Set[Symbol]()
		private[this] var toReload = newReloadMap()
		private[this] var toForget = mutable.Set[Symbol]()
		private[this] var callback0: AnalysisCallback = null
		def callback: AnalysisCallback = callback0
	}
}