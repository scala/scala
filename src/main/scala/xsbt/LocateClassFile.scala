/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import scala.tools.nsc.symtab.Flags
import scala.tools.nsc.io.AbstractFile

import java.io.File

/**
 * Contains utility methods for looking up class files corresponding to Symbols.
 */
abstract class LocateClassFile extends Compat {
  val global: CallbackGlobal
  import global._

  private[this] final val classSeparator = '.'
  protected def classFile(sym: Symbol): Option[(AbstractFile, String, Boolean)] =
    // package can never have a corresponding class file; this test does not
    // catch package objects (that do not have this flag set)
    if (sym hasFlag scala.tools.nsc.symtab.Flags.PACKAGE) None else {
      import scala.tools.nsc.symtab.Flags
      val name = flatname(sym, classSeparator) + moduleSuffix(sym)
      findClass(name).map { case (file, inOut) => (file, name, inOut) } orElse {
        if (isTopLevelModule(sym)) {
          val linked = sym.companionClass
          if (linked == NoSymbol)
            None
          else
            classFile(linked)
        } else
          None
      }
    }
  private def flatname(s: Symbol, separator: Char) =
    atPhase(currentRun.flattenPhase.next) { s fullName separator }

  protected def isTopLevelModule(sym: Symbol): Boolean =
    atPhase(currentRun.picklerPhase.next) {
      sym.isModuleClass && !sym.isImplClass && !sym.isNestedClass
    }
  protected def className(s: Symbol, sep: Char, dollarRequired: Boolean): String =
    flatname(s, sep) + (if (dollarRequired) "$" else "")
  protected def fileForClass(outputDirectory: File, s: Symbol, separatorRequired: Boolean): File =
    new File(outputDirectory, className(s, File.separatorChar, separatorRequired) + ".class")
}
