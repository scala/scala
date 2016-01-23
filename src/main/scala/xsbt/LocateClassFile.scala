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
abstract class LocateClassFile extends Compat with ClassName {
  val global: CallbackGlobal
  import global._

  private[this] final val classSeparator = '.'
  protected def classFile(sym: Symbol): Option[(AbstractFile, String, Boolean)] =
    // package can never have a corresponding class file; this test does not
    // catch package objects (that do not have this flag set)
    if (sym hasFlag scala.tools.nsc.symtab.Flags.PACKAGE) None else {
      import scala.tools.nsc.symtab.Flags
      val binaryClassName = flatname(sym, classSeparator) + moduleSuffix(sym)
      findClass(binaryClassName).map { case (file, inOut) => (file, binaryClassName, inOut) } orElse {
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

  protected def fileForClass(outputDirectory: File, s: Symbol, separatorRequired: Boolean): File =
    new File(outputDirectory, flatclassName(s, File.separatorChar, separatorRequired) + ".class")
}
