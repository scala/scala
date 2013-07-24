/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import scala.tools.nsc.{io, plugins, symtab, Global, Phase}
import io.{AbstractFile, PlainFile, ZipArchive}
import plugins.{Plugin, PluginComponent}
import scala.collection.mutable.{HashMap, HashSet, Map, Set}

import java.io.File
import java.util.zip.ZipFile
import xsbti.AnalysisCallback

object Analyzer
{
	def name = "xsbt-analyzer"
}
final class Analyzer(val global: CallbackGlobal) extends LocateClassFile
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
					def binaryDependency(file: File, className: String) = callback.binaryDependency(file, className, sourceFile, inherited)
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
						callback.sourceDependency(onSource.file, sourceFile, inherited)
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

}

