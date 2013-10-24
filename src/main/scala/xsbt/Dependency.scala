/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import scala.tools.nsc.{io, symtab, Phase}
import io.{AbstractFile, PlainFile, ZipArchive}
import symtab.Flags

import java.io.File

object Dependency
{
	def name = "xsbt-dependency"
}
/**
 * Extracts dependency information from each compilation unit.
 *
 * This phase uses CompilationUnit.depends and CallbackGlobal.inheritedDependencies
 * to collect all symbols that given compilation unit depends on. Those symbols are
 * guaranteed to represent Class-like structures.
 *
 * The CallbackGlobal.inheritedDependencies is populated by the API phase. See,
 * ExtractAPI class.
 *
 * When dependency symbol is processed, it is mapped back to either source file where
 * it's defined in (if it's available in current compilation run) or classpath entry
 * where it originates from. The Symbol->Classfile mapping is implemented by
 * LocateClassFile that we inherit from.
 */
final class Dependency(val global: CallbackGlobal) extends LocateClassFile
{
	import global._

	def newPhase(prev: Phase): Phase = new DependencyPhase(prev)
	private class DependencyPhase(prev: Phase) extends Phase(prev)
	{
		override def description = "Extracts dependency information"
		def name = Dependency.name
		def run
		{
			for(unit <- currentRun.units if !unit.isJava)
			{
				// build dependencies structure
				val sourceFile = unit.source.file.file
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
			}
		}
	}

}
