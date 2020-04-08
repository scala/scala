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

package scala.tools.nsc.tasty

import scala.tools.nsc.tasty.TastyName.{ModuleName, QualifiedName, SimpleName}

case class ErasedTypeRef(arrayDims: Int, qualifiedName: String, isModule: Boolean) {
  def signature: String = {
    s"${"[" * arrayDims}${if (isModule) s"object $qualifiedName" else qualifiedName}"
  }
}

object ErasedTypeRef {

  def apply(tname: TastyName): Option[ErasedTypeRef] = {

    def name(pkg: String, terminal: String) = if (pkg.isEmpty) terminal else s"$pkg.$terminal"

    def specialised(arrayDims: Int, pkg: String, terminal: String, isModule: Boolean): ErasedTypeRef = terminal match {
      case s"$inner[]" => specialised(arrayDims + 1, pkg, inner, isModule)
      case clazz       => ErasedTypeRef(arrayDims, name(pkg, clazz), isModule)
    }

    var isModule = false

    val classKind = tname match {
      case ModuleName(classKind) =>
        isModule = true
        classKind
      case nonModule => nonModule
    }

    classKind match {
      case terminal: SimpleName => // unqualified in the <empty> package
        Some(specialised(0, "", terminal.raw, isModule))
      case QualifiedName(path, TastyName.PathSep, terminal) =>
        Some(specialised(0, path.source, terminal.raw, isModule))
      case _ =>
        None
    }

  }

}
