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
final class Analyzer(val global: Global, val callback: AnalysisCallback) extends Compat
{
	import global._

	def newPhase(prev: Phase): Phase = new AnalyzerPhase(prev)
	private class AnalyzerPhase(prev: Phase) extends Phase(prev)
	{
		override def description = "Extracts dependency information, finds concrete instances of provided superclasses, and application entry points."
		def name = Analyzer.name
		def run
		{
			val outputDirectory = new File(global.settings.outdir.value)

			for(unit <- currentRun.units)
			{
				// build dependencies structure
				val sourceFile = unit.source.file.file
				callback.beginSource(sourceFile)
				for(on <- unit.depends)
				{
					def binaryDependency(file: File, className: String) = callback.binaryDependency(file, className, sourceFile)
					val onSource = on.sourceFile
					if(onSource == null)
					{
						classFile(on) match
						{
							case Some((f,className)) =>
								f match
								{
									case ze: ZipArchive#Entry => binaryDependency(new File(archive(ze).getName), className)
									case pf: PlainFile => binaryDependency(pf.file, className)
									case _ => ()
								}
							case None => ()
						}
					}
					else
						callback.sourceDependency(onSource.file, sourceFile)
				}

				// build list of generated classes
				for(iclass <- unit.icode)
				{
					val sym = iclass.symbol
					def addGenerated(separatorRequired: Boolean)
					{
						val classFile = fileForClass(outputDirectory, sym, separatorRequired)
						if(classFile.exists)
							callback.generatedClass(sourceFile, classFile)
					}
					if(sym.isModuleClass && !sym.isImplClass)
					{
						if(isTopLevelModule(sym) && linkedClass(sym) == NoSymbol)
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

	private def classFile(sym: Symbol): Option[(AbstractFile, String)] =
	{
		import scala.tools.nsc.symtab.Flags
		val name = flatname(sym, finder.classSeparator) + moduleSuffix(sym)
		finder.findClass(name).map(file => (file, name))  orElse {
			if(isTopLevelModule(sym))
			{
				val linked = linkedClass(sym)
				if(linked == NoSymbol)
					None
				else
					classFile(linked)
			}
			else
				None
		}
	}
	// doesn't seem to be in 2.7.7, so copied from GenJVM to here
	private def moduleSuffix(sym: Symbol) =
		if (sym.hasFlag(Flags.MODULE) && !sym.isMethod && !sym.isImplClass && !sym.hasFlag(Flags.JAVA)) "$" else "";
	private def flatname(s: Symbol, separator: Char) =
		atPhase(currentRun.flattenPhase.next) { nameString(s, separator) }

	private def isTopLevelModule(sym: Symbol): Boolean =
		atPhase (currentRun.picklerPhase.next) {
			sym.isModuleClass && !sym.isImplClass && !sym.isNestedClass
		}
	private def fileForClass(outputDirectory: File, s: Symbol, separatorRequired: Boolean): File =
		new File(outputDirectory, flatname(s, File.separatorChar) + (if(separatorRequired) "$" else "") + ".class")

	// required because the 2.8 way to find a class is:
	//   classPath.findClass(name).flatMap(_.binary)
	// and the 2.7 way is:
	//   val entry = classPath.root.find(name, false)
	//   if(entry eq null) None else Some(entry.classFile)
	private lazy val finder = try { new LegacyFinder } catch { case _ => new NewFinder }
	private trait ClassFinder
	{
		def classSeparator: Char
		def findClass(name: String): Option[AbstractFile]
	}
	private class NewFinder extends ClassFinder
	{
		private class Compat27 { def findClass(name: String) = this; def flatMap(f: Compat27 => AnyRef) = Predef.error("Should never be called"); def binary = None }
		private implicit def compat27(any: AnyRef): Compat27 = new Compat27

		def classSeparator = '.' // 2.8 uses . when searching for classes
		def findClass(name: String): Option[AbstractFile] =
			classPath.findClass(name).flatMap(_.binary.asInstanceOf[Option[AbstractFile]])
	}
	private class LegacyFinder extends ClassFinder
	{
		private class Compat28 { def root: Compat28 = invalid; def find(n: String, b: Boolean) = this; def classFile = invalid; def invalid = Predef.error("Should never be called") }
		private implicit def compat28(any: AnyRef): Compat28 = new Compat28

		def classSeparator = File.separatorChar // 2.7 uses / or \ when searching for classes
		private val root = classPath.root
		def findClass(name: String): Option[AbstractFile] =
		{
			val entry = root.find(name, false)
			if(entry eq null) None else Some(entry.classFile)
		}
	}
}
abstract class Compat
{
	val global: Global
	import global._
	def archive(s: ZipArchive#Entry): ZipFile = s.getArchive
	def nameString(s: Symbol): String = s.fullNameString
	def nameString(s: Symbol, sep: Char): String = s.fullNameString(sep)
	def isExistential(s: Symbol): Boolean = s.isExistential
	def isNonClassType(s: Symbol): Boolean = s.isTypeMember
	val LocalChild = global.tpnme.LOCAL_CHILD
	val Nullary = global.NullaryMethodType

	def linkedClass(s: Symbol): Symbol = s.linkedClassOfModule

	/** After 2.8.0.Beta1, fullNameString was renamed fullName.
	* linkedClassOfModule was renamed companionClass. */
	private[this] implicit def symCompat(sym: Symbol): SymCompat = new SymCompat(sym)
	private[this] final class SymCompat(s: Symbol)
	{
		def fullNameString = s.fullName; def fullName = sourceCompatibilityOnly
		def fullNameString(sep: Char) = s.fullName(sep); def fullName(sep: Char) = sourceCompatibilityOnly
		
		def isExistential: Boolean = s.isExistentiallyBound; def isExistentiallyBound = sourceCompatibilityOnly
		def isTypeMember: Boolean = s.isNonClassType; def isNonClassType = sourceCompatibilityOnly
		
		def linkedClassOfModule = s.companionClass; def companionClass = sourceCompatibilityOnly
	// In 2.8, hasAttribute is renamed to hasAnnotation
		def hasAnnotation(a: Symbol) = s.hasAttribute(a); def hasAttribute(a: Symbol) = sourceCompatibilityOnly
	}
	private[this] final class MiscCompat
	{
		// in 2.9, nme.LOCALCHILD was renamed to tpnme.LOCAL_CHILD
		def tpnme = nme
		def LOCAL_CHILD = nme.LOCALCHILD
		def LOCALCHILD = sourceCompatibilityOnly

		def NullaryMethodType = NullaryMethodTpe
	}
	// in 2.9, NullaryMethodType was added to Type
	object NullaryMethodTpe {
		def unapply(t: Type): Option[Type] = None
	}

	final def hasAnnotation(s: Symbol)(ann: Symbol) = atPhase(currentRun.typerPhase) { s.hasAnnotation(ann) }

	/** After 2.8.0.Beta1, getArchive was renamed archive.*/
	private[this] implicit def zipCompat(z: ZipArchive#Entry): ZipCompat = new ZipCompat(z)
	private[this] final class ZipCompat(z: ZipArchive#Entry)
	{
		def getArchive = z.archive; def archive = sourceCompatibilityOnly
	}
	private[this] def sourceCompatibilityOnly: Nothing = throw new RuntimeException("For source compatibility only: should not get here.")

	private[this] final implicit def miscCompat(n: AnyRef): MiscCompat = new MiscCompat
}