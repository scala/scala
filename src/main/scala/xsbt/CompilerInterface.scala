/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import xsbti.{AnalysisCallback,Logger,Problem,Reporter,Severity}
import xsbti.compile._
import scala.tools.nsc.{backend, io, reporters, symtab, util, Phase, Global, Settings, SubComponent}
import scala.tools.nsc.interactive.RangePositions
import backend.JavaPlatform
import scala.tools.util.PathResolver
import symtab.SymbolLoaders
import util.{ClassPath,DirectoryClassPath,MergedClassPath,JavaClassPath}
import ClassPath.{ClassPathContext,JavaContext}
import io.AbstractFile
import scala.annotation.tailrec
import scala.collection.mutable
import Log.debug
import java.io.File

final class CompilerInterface
{
	def newCompiler(options: Array[String], output: Output, initialLog: Logger, initialDelegate: Reporter, resident: Boolean): CachedCompiler =
		new CachedCompiler0(options, output, new WeakLog(initialLog, initialDelegate), resident)

	def run(sources: Array[File], changes: DependencyChanges, callback: AnalysisCallback, log: Logger, delegate: Reporter, progress: CompileProgress, cached: CachedCompiler): Unit =
		cached.run(sources, changes, callback, log, delegate, progress)
}
// for compatibility with Scala versions without Global.registerTopLevelSym (2.8.1 and earlier)
sealed trait GlobalCompat { self: Global =>
	def registerTopLevelSym(sym: Symbol): Unit
	sealed trait RunCompat {
		def informUnitStarting(phase: Phase, unit: CompilationUnit) {}
	}
}
sealed abstract class CallbackGlobal(settings: Settings, reporter: reporters.Reporter, output: Output) extends Global(settings, reporter) with GlobalCompat {
	def callback: AnalysisCallback
	def findClass(name: String): Option[(AbstractFile,Boolean)]
	lazy val outputDirs: Iterable[File] = {
		output match {
			case single: SingleOutput => List(single.outputDirectory)
			case multi: MultipleOutput => multi.outputGroups.toStream map (_.outputDirectory)
		}
	}
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

private final class CachedCompiler0(args: Array[String], output: Output, initialLog: WeakLog, resident: Boolean) extends CachedCompiler
{
	val settings = new Settings(s => initialLog(s))
	output match {
		case multi: MultipleOutput =>
			for (out <- multi.outputGroups)
				settings.outputDirs.add(out.sourceDirectory.getAbsolutePath, out.outputDirectory.getAbsolutePath)
		case single: SingleOutput =>
			settings.outputDirs.setSingleOutput(single.outputDirectory.getAbsolutePath)
	}

	val command = Command(args.toList, settings)
	private[this] val dreporter = DelegatingReporter(settings, initialLog.reporter)
	try {
		if(!noErrors(dreporter)) {
			dreporter.printSummary()
			handleErrors(dreporter, initialLog.logger)
		}
	} finally
		initialLog.clear()

	def noErrors(dreporter: DelegatingReporter) = !dreporter.hasErrors && command.ok

	def run(sources: Array[File], changes: DependencyChanges, callback: AnalysisCallback, log: Logger, delegate: Reporter, progress: CompileProgress): Unit = synchronized
	{
		debug(log, "Running cached compiler " + hashCode.toHexString + ", interfacing (CompilerInterface) with Scala compiler " + scala.tools.nsc.Properties.versionString)
		val dreporter = DelegatingReporter(settings, delegate)
		try { run(sources.toList, changes, callback, log, dreporter, progress) }
		finally { dreporter.dropDelegate() }
	}
	private[this] def run(sources: List[File], changes: DependencyChanges, callback: AnalysisCallback, log: Logger, dreporter: DelegatingReporter, compileProgress: CompileProgress)
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
				val run = new compiler.Run with compiler.RunCompat {
					override def informUnitStarting(phase: Phase, unit: compiler.CompilationUnit) {
						compileProgress.startUnit(phase.name, unit.source.path)
					}
					override def progress(current: Int, total: Int) {
						if (!compileProgress.advance(current, total))
							cancel
					}
				}
				if (resident) compiler.reload(changes)
				val sortedSourceFiles = sources.map(_.getAbsolutePath).sortWith(_ < _)
				run compile sortedSourceFiles
				processUnreportedWarnings(run)
			} finally {
				if(resident) compiler.clear()
			}
			dreporter.problems foreach { p => callback.problem(p.category, p.position, p.message, p.severity, true) }
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
			// allConditionalWarnings and the ConditionalWarning class are only in 2.10+
			final class CondWarnCompat(val what: String, val warnings: mutable.ListBuffer[(compiler.Position, String)])
			implicit def compat(run: AnyRef): Compat = new Compat
			final class Compat { def allConditionalWarnings = List[CondWarnCompat]() }

		val warnings = run.allConditionalWarnings
		if(!warnings.isEmpty)
			compiler.logUnreportedWarnings(warnings.map(cw => ("" /*cw.what*/, cw.warnings.toList)))
	}

	val compiler: Compiler = {
		if (command.settings.Yrangepos.value)
			new Compiler() with RangePositions
		else
			new Compiler()
	}
	class Compiler extends CallbackGlobal(command.settings, dreporter, output)
	{
		object dummy // temporary fix for #4426
		object sbtAnalyzer extends
		{
			val global: Compiler.this.type = Compiler.this
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
		/** This phase walks trees and constructs a representation of the public API, which is used for incremental recompilation.
		 *
		 * We extract the api after picklers, since that way we see the same symbol information/structure
		 * irrespective of whether we were typechecking from source / unpickling previously compiled classes.
		 */
		object apiExtractor extends
		{
			val global: Compiler.this.type = Compiler.this
			val phaseName = API.name
			val runsAfter = List("typer")
			override val runsBefore = List("erasure")
			// allow apiExtractor's phase to be overridden using the sbt.api.phase property
			// (in case someone would like the old timing, which was right after typer)
			// TODO: consider migrating to simply specifying "pickler" for `runsAfter` and "uncurry" for `runsBefore`
			val runsRightAfter = Option(System.getProperty("sbt.api.phase")) orElse Some("pickler")
		}
		with SubComponent
		{
			val api = new API(global)
			def newPhase(prev: Phase) = api.newPhase(prev)
			def name = phaseName
		}

		override lazy val phaseDescriptors =
		{
			phasesSet += sbtAnalyzer
			phasesSet += apiExtractor
			superComputePhaseDescriptors
		}
		// Required because computePhaseDescriptors is private in 2.8 (changed to protected sometime later).
		private[this] def superComputePhaseDescriptors() = superCall("computePhaseDescriptors").asInstanceOf[List[SubComponent]]
		private[this] def superDropRun(): Unit =
			try { superCall("dropRun") } catch { case e: NoSuchMethodException => () } // dropRun not in 2.8.1
		private[this] def superCall(methodName: String): AnyRef =
		{
			val meth = classOf[Global].getDeclaredMethod(methodName)
			meth.setAccessible(true)
			meth.invoke(this)
		}
		def logUnreportedWarnings(seq: Seq[(String, List[(Position,String)])]): Unit = // Scala 2.10.x and later
		{
			val drep = reporter.asInstanceOf[DelegatingReporter]
			for( (what, warnings) <- seq; (pos, msg) <- warnings) yield
				callback.problem(what, drep.convert(pos), msg, Severity.Warn, false)
		}
		
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

		def findClass(name: String): Option[(AbstractFile, Boolean)] =
			getOutputClass(name).map(f => (f,true)) orElse findOnClassPath(name).map(f =>(f, false))

		def getOutputClass(name: String): Option[AbstractFile] =
		{
			// This could be improved if a hint where to look is given.
			val className = name.replace('.', '/') + ".class"
			outputDirs map (new File(_, className)) find (_.exists) map (AbstractFile.getFile(_))
		}

		def findOnClassPath(name: String): Option[AbstractFile] =
			classPath.findClass(name).flatMap(_.binary.asInstanceOf[Option[AbstractFile]])

		override def registerTopLevelSym(sym: Symbol) = toForget += sym

		final def unlinkAll(m: Symbol) {
			val scope = m.owner.info.decls
			scope unlink m
			scope unlink m.companionSymbol
		}

		def forgetAll()
		{
			for(sym <- toForget)
				unlinkAll(sym)
			toForget = mutable.Set()
		}

		// fine-control over external changes is unimplemented:
		//   must drop whole CachedCompiler when !changes.isEmpty
		def reload(changes: DependencyChanges)
		{
				for ((_,out) <- settings.outputDirs.outputs) inv(out.path)
		}

		private[this] var toForget = mutable.Set[Symbol]()
		private[this] var callback0: AnalysisCallback = null
		def callback: AnalysisCallback = callback0

		private[this] def defaultClasspath = new PathResolver(settings).result

		// override defaults in order to inject a ClassPath that can change
		override lazy val platform = new PlatformImpl
		override lazy val classPath = if(resident) classPathCell else defaultClasspath
		private[this] lazy val classPathCell = new ClassPathCell(defaultClasspath)

		final class PlatformImpl extends JavaPlatform
		{
			val global: Compiler.this.type = Compiler.this
			// This can't be overridden to provide a ClassPathCell, so we have to fix it to the initial classpath
			// This is apparently never called except by rootLoader, so we can return the default and warn if someone tries to use it.
			override lazy val classPath = {
				if(resident)
					compiler.warning("platform.classPath should not be called because it is incompatible with sbt's resident compilation.  Use Global.classPath")
				defaultClasspath
			}
			override def rootLoader = if(resident) newPackageLoaderCompat(rootLoader)(compiler.classPath) else super.rootLoader
		}

		private[this] type PlatformClassPath = ClassPath[AbstractFile]
		private[this] type OptClassPath = Option[PlatformClassPath]

		// converted from Martin's new code in scalac for use in 2.8 and 2.9
		private[this] def inv(path: String)
		{
			classPathCell.delegate match {
				case cp: util.MergedClassPath[_] =>
					val dir = AbstractFile getDirectory path
					val canonical = dir.file.getCanonicalPath
					def matchesCanonicalCompat(e: ClassPath[_]) = e.origin.exists { opath =>
							(AbstractFile getDirectory opath).file.getCanonicalPath == canonical
					}

					cp.entries find matchesCanonicalCompat match {
						case Some(oldEntry) =>
							val newEntry = cp.context.newClassPath(dir)
							classPathCell.updateClassPath(oldEntry, newEntry)
							reSyncCompat(definitions.RootClass, Some(classPath), Some(oldEntry), Some(newEntry))
						case None =>
							error("Cannot invalidate: no entry named " + path + " in classpath " + classPath)
					}
			}
		}
		private def reSyncCompat(root: ClassSymbol, allEntries: OptClassPath, oldEntry: OptClassPath, newEntry: OptClassPath)
		{
			val getName: PlatformClassPath => String = (_.name)
			def hasClasses(cp: OptClassPath) = cp.exists(_.classes.nonEmpty)
			def invalidateOrRemove(root: ClassSymbol) =
				allEntries match {
					case Some(cp) => root setInfo newPackageLoader0[Type](cp)
					case None => root.owner.info.decls unlink root.sourceModule
				}

			def packageNames(cp: PlatformClassPath): Set[String] = cp.packages.toSet map getName
			def subPackage(cp: PlatformClassPath, name: String): OptClassPath =
				cp.packages find (_.name == name)

			val classesFound = hasClasses(oldEntry) || hasClasses(newEntry)
			if (classesFound && !isSystemPackageClass(root)) {
				invalidateOrRemove(root)
			} else {
				if (classesFound && root.isRoot)
					invalidateOrRemove(definitions.EmptyPackageClass.asInstanceOf[ClassSymbol])
				(oldEntry, newEntry) match {
					case (Some(oldcp) , Some(newcp)) =>
						for (pstr <- packageNames(oldcp) ++ packageNames(newcp)) {
							val pname = newTermName(pstr)
							var pkg = root.info decl pname
							if (pkg == NoSymbol) {
								// package was created by external agent, create symbol to track it
								assert(!subPackage(oldcp, pstr).isDefined)
								pkg = enterPackageCompat(root, pname, newPackageLoader0[loaders.SymbolLoader](allEntries.get))
							}
							reSyncCompat(
									pkg.moduleClass.asInstanceOf[ClassSymbol],
									subPackage(allEntries.get, pstr), subPackage(oldcp, pstr), subPackage(newcp, pstr))
						}
					case (Some(oldcp), None) => invalidateOrRemove(root)
					case (None, Some(newcp)) => invalidateOrRemove(root)
					case (None, None) => ()
				}
			}
		}
		private[this] def enterPackageCompat(root: ClassSymbol, pname: Name, completer: loaders.SymbolLoader): Symbol =
		{
			val pkg = root.newPackage(pname)
			pkg.moduleClass.setInfo(completer)
			pkg.setInfo(pkg.moduleClass.tpe)
			root.info.decls.enter(pkg)
			pkg
		}

		// type parameter T, `dummy` value for inference, and reflection are source compatibility hacks
		//   to work around JavaPackageLoader and PackageLoader changes between 2.9 and 2.10 
		//   and in particular not being able to say JavaPackageLoader in 2.10 in a compatible way (it no longer exists)
		private[this] def newPackageLoaderCompat[T](dummy: => T)(classpath: ClassPath[AbstractFile]): T =
			newPackageLoader0[T](classpath)

		private[this] def newPackageLoader0[T](classpath: ClassPath[AbstractFile]): T =
			loaderClassCompat.getConstructor(classOf[SymbolLoaders], classOf[ClassPath[AbstractFile]]).newInstance(loaders, classpath).asInstanceOf[T]

		private[this] lazy val loaderClassCompat: Class[_] =
			try Class.forName("scala.tools.nsc.symtab.SymbolLoaders$JavaPackageLoader")
			catch { case e: Exception =>
				Class.forName("scala.tools.nsc.symtab.SymbolLoaders$PackageLoader")
			}

		private[this] implicit def newPackageCompat(s: ClassSymbol): NewPackageCompat = new NewPackageCompat(s)
		private[this] final class NewPackageCompat(s: ClassSymbol) {
			def newPackage(name: Name): Symbol = s.newPackage(NoPosition, name)
			def newPackage(pos: Position, name: Name): Nothing = throw new RuntimeException("source compatibility only")
		}
		private[this] def isSystemPackageClass(pkg: Symbol) =
			pkg == definitions.RootClass ||
			pkg == definitions.ScalaPackageClass || {
				val pkgname = pkg.fullName
				(pkgname startsWith "scala.") && !(pkgname startsWith "scala.tools")
			}

		final class ClassPathCell[T](var delegate: MergedClassPath[T]) extends ClassPath[T] {
			private[this] class DeltaClassPath[T](original: MergedClassPath[T], oldEntry: ClassPath[T], newEntry: ClassPath[T]) 
				extends MergedClassPath[T](original.entries map (e => if (e == oldEntry) newEntry else e), original.context)
		
			def updateClassPath(oldEntry: ClassPath[T], newEntry: ClassPath[T]) {
				delegate = new DeltaClassPath(delegate, oldEntry, newEntry)
			}

			def name = delegate.name
			override def origin = delegate.origin
			def asURLs = delegate.asURLs
			def asClasspathString = delegate.asClasspathString
			def context = delegate.context
			def classes = delegate.classes
			def packages = delegate.packages
			def sourcepaths = delegate.sourcepaths
		}
	}
}
