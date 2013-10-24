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
