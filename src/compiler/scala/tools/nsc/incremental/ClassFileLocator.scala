/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools
package nsc
package incremental

import scala.reflect.io.NoAbstractFile
import scala.tools.nsc.io.AbstractFile

import java.io.File

/**
 * Contains utility methods for looking up class files corresponding to Symbols.
 */
class ClassFileLocator[G <: ZincGlobal](val global: G) {
  import global._

  private[this] final val classSeparator = '.'
  def classFile(sym: Symbol): Option[(AbstractFile, String)] =
    // package can never have a corresponding class file; this test does not
    // catch package objects (that do not have this flag set)
    if (sym hasFlag scala.tools.nsc.symtab.Flags.PACKAGE) None
    else {
      val file = sym.associatedFile

      if (file == NoAbstractFile) {
        if (classNameUtils.isTopLevelModule(sym)) {
          val linked = sym.companionClass
          if (linked == NoSymbol)
            None
          else
            classFile(linked)
        } else
          None
      } else {
        Some((file, classNameUtils.flatname(sym, classSeparator) + sym.moduleSuffix))
      }
    }

  def fileForClass(outputDirectory: File, s: Symbol, addModuleSuffix: Boolean): Option[File] = {
    val f = new File(outputDirectory, classNameUtils.flatclassName(s, File.separatorChar, addModuleSuffix) + ".class")
    if (f.exists()) Some(f) else None
  }


  def pathToClassFile(s: Symbol, addModuleSuffix: Boolean): String =
    classNameUtils.flatclassName(s, File.separatorChar, addModuleSuffix) + ".class"
}
