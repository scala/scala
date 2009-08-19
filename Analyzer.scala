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
import xsbti.{AnalysisCallback, AnalysisCallbackContainer}

class Analyzer(val global: Global) extends Plugin
{
	val callback = global.asInstanceOf[AnalysisCallbackContainer].analysisCallback

	import global._

	val name = "xsbt-analyze"
	val description = "A plugin to find all concrete instances of a given class and extract dependency information."
	val components = List[PluginComponent](Component)

	/* ================================================== */
	// These two templates abuse scope for source compatibility between Scala 2.7.x and 2.8.x so that a single
	// sbt codebase compiles with both series of versions.
	// In 2.8.x, PluginComponent.runsAfter has type List[String] and the method runsBefore is defined on
	//   PluginComponent with default value Nil.
	// In 2.7.x, runsBefore does not exist on PluginComponent and PluginComponent.runsAfter has type String.
	//
	// Therefore, in 2.8.x, object runsBefore is shadowed by PluginComponent.runsBefore (which is Nil) and so
	//   afterPhase :: runsBefore
	// is equivalent to List[String](afterPhase)
	// In 2.7.x, object runsBefore is not shadowed and so runsAfter has type String.
	private object runsBefore { def :: (s: String) = s }
	private abstract class CompatiblePluginComponent(afterPhase: String) extends PluginComponent
	{
		override val runsAfter = afterPhase :: runsBefore
	}
	/* ================================================== */

	private object Component extends CompatiblePluginComponent("jvm")
	{
		val global = Analyzer.this.global
		val phaseName = Analyzer.this.name
		def newPhase(prev: Phase) = new AnalyzerPhase(prev)
	}

	private class AnalyzerPhase(prev: Phase) extends Phase(prev)
	{
		def name = Analyzer.this.name
		def run
		{
			val outputDirectory = new File(global.settings.outdir.value)
			val superclasses = callback.superclassNames flatMap(classForName)

			for(unit <- currentRun.units)
			{
				// build dependencies structure
				val sourceFile = unit.source.file.file
				callback.beginSource(sourceFile)
				for(on <- unit.depends)
				{
					val onSource = on.sourceFile
					if(onSource == null)
					{
						classFile(on) match
						{
							case Some(f) =>
							{
								f match
								{
									case ze: ZipArchive#Entry => callback.jarDependency(new File(ze.getArchive.getName), sourceFile)
									case pf: PlainFile => callback.classDependency(pf.file, sourceFile)
									case _ => ()
								}
							}
							case None => ()
						}
					}
					else
						callback.sourceDependency(onSource.file, sourceFile)
				}

				// find subclasses and modules with main methods
				for(clazz @ ClassDef(mods, n, _, _) <- unit.body)
				{
					val sym = clazz.symbol
					if(sym != NoSymbol && mods.isPublic && !mods.isAbstract && !mods.isTrait &&
						 !sym.isImplClass && sym.isStatic && !sym.isNestedClass)
					{
						val isModule = sym.isModuleClass
						for(superclass <- superclasses.filter(sym.isSubClass))
							callback.foundSubclass(sourceFile, sym.fullNameString, superclass.fullNameString, isModule)
						if(isModule && hasMainMethod(sym))
							callback.foundApplication(sourceFile, sym.fullNameString)
					}
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
						if(isTopLevelModule(sym) && sym.linkedClassOfModule == NoSymbol)
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

	private def classForName(name: String) =
	{
		try
		{
			if(name.indexOf('.') < 0)
			{
				val sym = definitions.EmptyPackageClass.info.member(newTypeName(name))
				if(sym != NoSymbol) Some( sym ) else { callback.superclassNotFound(name); None }
			}
			else
				Some( global.definitions.getClass(newTermName(name)) )
		}
		catch { case fe: scala.tools.nsc.FatalError =>  callback.superclassNotFound(name); None }
	}
	private def classFile(sym: Symbol): Option[AbstractFile] =
	{
		import scala.tools.nsc.symtab.Flags
		val name = sym.fullNameString(File.separatorChar) + (if (sym.hasFlag(Flags.MODULE)) "$" else "")
		val entry = classPath.root.find(name, false)
		if (entry ne null)
			Some(entry.classFile)
		else if(isTopLevelModule(sym))
		{
			val linked = sym.linkedClassOfModule
			if(linked == NoSymbol)
				None
			else
				classFile(linked)
		}
		else
			None
	}

	private def isTopLevelModule(sym: Symbol): Boolean =
		atPhase (currentRun.picklerPhase.next) {
			sym.isModuleClass && !sym.isImplClass && !sym.isNestedClass
		}
	private def fileForClass(outputDirectory: File, s: Symbol, separatorRequired: Boolean): File =
		fileForClass(outputDirectory, s, separatorRequired, ".class")
	private def fileForClass(outputDirectory: File, s: Symbol, separatorRequired: Boolean, postfix: String): File =
	{
		if(s.owner.isPackageClass && s.isPackageClass)
			new File(packageFile(outputDirectory, s), postfix)
		else
			fileForClass(outputDirectory, s.owner.enclClass, true, s.simpleName + (if(separatorRequired) "$" else "") + postfix)
	}
	private def packageFile(outputDirectory: File, s: Symbol): File =
	{
		if(s.isEmptyPackageClass || s.isRoot)
			outputDirectory
		else
			new File(packageFile(outputDirectory, s.owner.enclClass), s.simpleName.toString)
	}

	private def hasMainMethod(sym: Symbol): Boolean =
	{
		val main = sym.info.nonPrivateMember(newTermName("main"))//nme.main)
		atPhase(currentRun.typerPhase.next) {
			main.tpe match
			{
				case OverloadedType(pre, alternatives) => alternatives.exists(alt => isVisible(alt) && isMainType(pre.memberType(alt)))
				case tpe => isVisible(main) && isMainType(main.owner.thisType.memberType(main))
			}
		}
	}
	private def isVisible(sym: Symbol) = sym != NoSymbol && sym.isPublic && !sym.isDeferred
	private def isMainType(tpe: Type): Boolean =
	{
		tpe match
		{
			// singleArgument is of type Symbol in 2.8.0 and type Type in 2.7.x
			case MethodType(List(singleArgument), result) => isUnitType(result) && isStringArray(singleArgument)
			case PolyType(typeParams, result) => isMainType(result)
			case _ =>  false
		}
	}
	private lazy val StringArrayType = appliedType(definitions.ArrayClass.typeConstructor, definitions.StringClass.tpe :: Nil)
	// isStringArray is overloaded to handle the incompatibility between 2.7.x and 2.8.0
	private def isStringArray(tpe: Type): Boolean = tpe =:= StringArrayType
	private def isStringArray(sym: Symbol): Boolean = isStringArray(sym.tpe)
	private def isUnitType(tpe: Type) = tpe.typeSymbol == definitions.UnitClass
}