/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package xsbt

import java.io.File

import scala.tools.nsc.Phase

object Analyzer {
  def name = "xsbt-analyzer"
}

final class Analyzer(val global: CallbackGlobal) extends LocateClassFile {
  import global._
  private val JarUtils = new JarUtils(outputDirs)

  def newPhase(prev: Phase): Phase = new AnalyzerPhase(prev)
  private class AnalyzerPhase(prev: Phase) extends GlobalPhase(prev) {
    override def description =
      "Finds concrete instances of provided superclasses, and application entry points."
    def name = Analyzer.name

    private lazy val existingClassesInJar: Set[JarUtils.ClassInJar] = {
      JarUtils.outputJar
        .map { jar =>
          val classes = JarUtils.listFiles(jar)
          classes.map(JarUtils.ClassInJar(jar, _))
        }
        .getOrElse(Set.empty)
    }

    def apply(unit: CompilationUnit): Unit = {
      if (!unit.isJava) {
        val sourceFile = unit.source.file.file
        for (iclass <- unit.icode) {
          val sym = iclass.symbol
          def addGenerated(separatorRequired: Boolean): Unit = {
            val locatedClass = if (JarUtils.isCompilingToJar) {
              locateClassInJar(sym, separatorRequired)
            } else {
              locatePlainClassFile(sym, separatorRequired)
            }

            locatedClass
              .foreach { classFile =>
                assert(sym.isClass, s"${sym.fullName} is not a class")
                // Use own map of local classes computed before lambdalift to ascertain class locality
                if (localToNonLocalClass.isLocal(sym).getOrElse(true)) {
                  // Inform callback about local classes, non-local classes have been reported in API
                  callback.generatedLocalClass(sourceFile, classFile)
                }
              }
          }

          if (sym.isModuleClass && !sym.isImplClass) {
            if (isTopLevelModule(sym) && sym.companionClass == NoSymbol)
              addGenerated(false)
            addGenerated(true)
          } else
            addGenerated(false)
        }
      }
    }

    private def locatePlainClassFile(sym: Symbol, separatorRequired: Boolean): Option[File] = {
      val outputDir = settings.outputDirs.outputDirFor(sym.sourceFile).file
      val classFile = fileForClass(outputDir, sym, separatorRequired)
      if (classFile.exists()) Some(classFile) else None
    }

    private def locateClassInJar(sym: Symbol, separatorRequired: Boolean): Option[File] = {
      val classFile =
        fileForClass(new File("."), sym, separatorRequired).toString
          .drop(2) // stripPrefix ./ or .\
      val classInJar = JarUtils.ClassInJar(classFile)
      if (existingClassesInJar.contains(classInJar)) {
        Some(new File(classInJar))
      } else {
        None
      }
    }
  }

}
