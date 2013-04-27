/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import scala.tools.nsc.{io, plugins, symtab, Global, Phase}
import io.{AbstractFile, PlainFile, ZipArchive}
import plugins.{Plugin, PluginComponent}
import symtab.Flags
import scala.collection.mutable.{HashMap, HashSet, Map, Set}

import java.io.File
import java.util.zip.ZipFile
import xsbti.AnalysisCallback

object Analyzer
{
	def name = "xsbt-analyzer"
}
final class Analyzer(val global: CallbackGlobal) extends Compat
{
	import global._

	def newPhase(prev: Phase): Phase = new AnalyzerPhase(prev)
	private class AnalyzerPhase(prev: Phase) extends Phase(prev)
	{
		override def description = "Extracts dependency information, finds concrete instances of provided superclasses, and application entry points."
		def name = Analyzer.name
		def run
		{
			for(unit <- currentRun.units if !unit.isJava)
			{
				// build dependencies structure
				val sourceFile = unit.source.file.file
				callback.beginSource(sourceFile)
				for(on <- unit.depends) processDependency(on, inherited=false)
				for(on <- inheritedDependencies.getOrElse(sourceFile, Nil: Iterable[Symbol])) processDependency(on, inherited=true)
				def processDependency(on: Symbol, inherited: Boolean)
				{
					def binaryDependency(file: File, className: String) = callback.binaryDependency(file, className, sourceFile /*, inherited*/)
					val onSource = on.sourceFile
					if(onSource == null)
					{
						classFile(on) match
						{
							case Some((f,className,inOutDir)) =>
								if(inOutDir && on.isJavaDefined) registerTopLevelSym(on)
								f match
								{
									case ze: ZipArchive#Entry => for(zip <- ze.underlyingSource; zipFile <- Option(zip.file) ) binaryDependency(zipFile, className)
									case pf: PlainFile => binaryDependency(pf.file, className)
									case _ => ()
								}
							case None => ()
						}
					}
					else
						callback.sourceDependency(onSource.file, sourceFile /*, inherited*/)
				}

				// build list of generated classes
				for(iclass <- unit.icode)
				{
					val sym = iclass.symbol
					def addGenerated(separatorRequired: Boolean)
					{
						for(classFile <- outputDirs map (fileForClass(_, sym, separatorRequired)) find (_.exists))
							callback.generatedClass(sourceFile, classFile, className(sym, '.', separatorRequired))
					}
					if(sym.isModuleClass && !sym.isImplClass)
					{
						if(isTopLevelModule(sym) && sym.companionClass == NoSymbol)
							addGenerated(false)
						addGenerated(true)
					}
					else
						addGenerated(false)
				}
				callback.endSource(sourceFile)
			}
		}
	}

	private[this] final val classSeparator = '.'
	private[this] def classFile(sym: Symbol): Option[(AbstractFile, String, Boolean)] =
	// package can never have a corresponding class file; this test does not
	// catch package objects (that do not have this flag set)
	if (sym hasFlag scala.tools.nsc.symtab.Flags.PACKAGE) None else
	{
		import scala.tools.nsc.symtab.Flags
		val name = flatname(sym, classSeparator) + moduleSuffix(sym)
		findClass(name).map { case (file,inOut) => (file, name,inOut) } orElse {
			if(isTopLevelModule(sym))
			{
				val linked = sym.companionClass
				if(linked == NoSymbol)
					None
				else
					classFile(linked)
			}
			else
				None
		}
	}
	private def flatname(s: Symbol, separator: Char) =
		atPhase(currentRun.flattenPhase.next) { s fullName separator }

	private def isTopLevelModule(sym: Symbol): Boolean =
		atPhase (currentRun.picklerPhase.next) {
			sym.isModuleClass && !sym.isImplClass && !sym.isNestedClass
		}
	private def className(s: Symbol, sep: Char, dollarRequired: Boolean): String =
		flatname(s, sep) + (if(dollarRequired) "$" else "")
	private def fileForClass(outputDirectory: File, s: Symbol, separatorRequired: Boolean): File =
		new File(outputDirectory, className(s, File.separatorChar, separatorRequired) + ".class")
}
abstract class Compat
{
	val global: Global
	import global._
	val LocalChild = global.tpnme.LOCAL_CHILD
	val Nullary = global.NullaryMethodType
	val ScalaObjectClass = definitions.ScalaObjectClass	

	private[this] final class MiscCompat
	{
		// in 2.9, nme.LOCALCHILD was renamed to tpnme.LOCAL_CHILD
		def tpnme = nme
		def LOCAL_CHILD = nme.LOCALCHILD
		def LOCALCHILD = sourceCompatibilityOnly

		// in 2.10, ScalaObject was removed
		def ScalaObjectClass = definitions.ObjectClass

		def NullaryMethodType = NullaryMethodTpe

		def MACRO = DummyValue

		// in 2.10, sym.moduleSuffix exists, but genJVM.moduleSuffix(Symbol) does not
		def moduleSuffix(sym: Symbol): String = sourceCompatibilityOnly
	}
	// in 2.9, NullaryMethodType was added to Type
	object NullaryMethodTpe {
		def unapply(t: Type): Option[Type] = None
	}

	// before 2.10, sym.moduleSuffix doesn't exist, but genJVM.moduleSuffix does
	private[this] implicit def symbolCompat(sym: Symbol): SymbolCompat = new SymbolCompat(sym)
	private[this] final class SymbolCompat(sym: Symbol) {
		def moduleSuffix = genJVM.moduleSuffix(sym)
	}


	val DummyValue = 0
	def hasMacro(s: Symbol): Boolean =
	{
		val MACRO = Flags.MACRO // will be DummyValue for versions before 2.10
		MACRO != DummyValue && s.hasFlag(MACRO)
	}
	def moduleSuffix(s: Symbol): String = s.moduleSuffix

	private[this] def sourceCompatibilityOnly: Nothing = throw new RuntimeException("For source compatibility only: should not get here.")

	private[this] final implicit def miscCompat(n: AnyRef): MiscCompat = new MiscCompat
}
