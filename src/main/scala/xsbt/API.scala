/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package xsbt

import scala.tools.nsc.Phase
import scala.tools.nsc.symtab.Flags
import xsbti.api._

object API {
  val name = "xsbt-api"
}

final class API(val global: CallbackGlobal) extends GlobalHelpers {
  import global._

  def newPhase(prev: Phase) = new ApiPhase(prev)
  class ApiPhase(prev: Phase) extends GlobalPhase(prev) {
    override def description = "Extracts the public API from source files."
    def name = API.name
    override def run(): Unit =
      {
        val start = System.currentTimeMillis
        super.run()
        callback.apiPhaseCompleted()
        val stop = System.currentTimeMillis
        debuglog("API phase took : " + ((stop - start) / 1000.0) + " s")
      }

    def apply(unit: global.CompilationUnit): Unit = processUnit(unit)

    def processUnit(unit: CompilationUnit) = if (!unit.isJava) processScalaUnit(unit)
    def processScalaUnit(unit: CompilationUnit): Unit = {
      val sourceFile = unit.source.file.file
      debuglog("Traversing " + sourceFile)
      callback.startSource(sourceFile)
      val extractApi = new ExtractAPI[global.type](global, sourceFile)
      val traverser = new TopLevelHandler(extractApi)
      traverser.apply(unit.body)
      if (global.callback.nameHashing) {
        val extractUsedNames = new ExtractUsedNames[global.type](global)
        val allUsedNames = extractUsedNames.extract(unit)
        def showUsedNames(className: String, names: Iterable[String]): String =
          s"$className:\n\t${names.mkString(", ")}"
        debuglog("The " + sourceFile + " contains the following used names:\n" +
          allUsedNames.map((showUsedNames _).tupled).mkString("\n"))
        allUsedNames foreach {
          case (className: String, names: Iterable[String]) =>
            names foreach { (name: String) => callback.usedName(className, name) }
        }
      }
      val classApis = traverser.allNonLocalClasses

      classApis.foreach(callback.api(sourceFile, _))
    }
  }

  private final class TopLevelHandler(extractApi: ExtractAPI[global.type]) extends TopLevelTraverser {
    def allNonLocalClasses: Set[ClassLike] = {
      extractApi.allExtractedNonLocalClasses
    }
    def `class`(c: Symbol): Unit = {
      extractApi.extractAllClassesOf(c.owner, c)
    }
  }

  private abstract class TopLevelTraverser extends Traverser {
    def `class`(s: Symbol): Unit
    override def traverse(tree: Tree): Unit = {
      tree match {
        case (_: ClassDef | _: ModuleDef) if isTopLevel(tree.symbol) => `class`(tree.symbol)
        case _: PackageDef =>
          super.traverse(tree)
        case _ =>
      }
    }
    def isTopLevel(sym: Symbol): Boolean = {
      !ignoredSymbol(sym) &&
        sym.isStatic &&
        !sym.isImplClass &&
        !sym.hasFlag(Flags.SYNTHETIC) &&
        !sym.hasFlag(Flags.JAVA) &&
        !sym.isNestedClass
    }
  }

}
