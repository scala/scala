/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package xsbt

import scala.reflect.io.NoAbstractFile
import scala.tools.nsc.symtab.Flags
import scala.tools.nsc.io.AbstractFile

import java.io.File

/**
 * Contains utility methods for looking up class files corresponding to Symbols.
 */
abstract class LocateClassFile extends ClassName {
  val global: CallbackGlobal
  import global._

  private[this] final val classSeparator = '.'
  protected def classFile(sym: Symbol): Option[(AbstractFile, String)] =
    // package can never have a corresponding class file; this test does not
    // catch package objects (that do not have this flag set)
    if (sym hasFlag scala.tools.nsc.symtab.Flags.PACKAGE) None else {
      val file = sym.associatedFile

      if (file == NoAbstractFile) {
        if (isTopLevelModule(sym)) {
          val linked = sym.companionClass
          if (linked == NoSymbol)
            None
          else
            classFile(linked)
        } else
          None
      } else {
        Some((file, flatname(sym, classSeparator) + sym.moduleSuffix))
      }
    }

  protected def fileForClass(outputDirectory: File, s: Symbol, separatorRequired: Boolean): File =
    new File(outputDirectory, flatclassName(s, File.separatorChar, separatorRequired) + ".class")
}
