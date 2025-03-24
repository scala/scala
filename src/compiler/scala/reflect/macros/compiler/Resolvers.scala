/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.reflect.macros
package compiler

trait Resolvers {
  self: DefaultMacroCompiler =>

  import global._
  import analyzer._
  import treeInfo._

  trait Resolver {
    self: MacroImplRefCompiler =>

    val isImplBundle: Boolean
    val isImplMethod = !isImplBundle

    lazy val looksCredible: Boolean = {
      val Applied(core, _, _) = untypedMacroImplRef
      typer.silent(_.typed(markMacroImplRef(core)), reportAmbiguousErrors = false).nonEmpty
    }

    lazy val (macroImplRef, isBlackbox, macroImplOwner, macroImpl, targs) =
      typer.silent(_.typed(markMacroImplRef(untypedMacroImplRef)), reportAmbiguousErrors = false) match {
        case SilentResultValue(macroImplRef @ MacroImplReference(_, isBlackbox, owner, meth, targs)) => (macroImplRef, isBlackbox, owner, meth, targs)
        case SilentResultValue(_) => MacroImplReferenceWrongShapeError()
        case ste: SilentTypeError => abort(ste.err.errPos, ste.err.errMsg)
      }
  }
}
