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

package scala.tools.tasty

import TastyName.{ObjectName, QualifiedName, SimpleName, TypeName, Empty, PathSep}

/** Represents an erased type of a scala class/object with the number of array dimensions.
  *
  * @param qualifiedName the fully qualified path of the class/object, including selection from package or class, unencoded
  * @param arrayDims the number of array dimensions of this type ref.
  *        A 0-dimensional array is just qualifiedName itself
  */
case class ErasedTypeRef(qualifiedName: TypeName, arrayDims: Int) {
  def signature: String = {
    val qualified = qualifiedName.source
    "[" * arrayDims + (if (qualifiedName.toTermName.isObjectName) s"object $qualified" else qualified)
  }
  def encode: ErasedTypeRef = ErasedTypeRef(TastyName.deepEncode(qualifiedName).toTypeName, arrayDims)
}

object ErasedTypeRef {

  def apply(tname: TastyName): ErasedTypeRef = {

    def name(qual: TastyName, tname: SimpleName, isModule: Boolean) = {
      val qualified = if (qual == Empty) tname else QualifiedName(qual, PathSep, tname)
      if (isModule) ObjectName(qualified) else qualified
    }

    def specialised(qual: TastyName, terminal: String, isModule: Boolean, arrayDims: Int = 0): ErasedTypeRef = terminal match {
      case s"$inner[]" => specialised(qual, inner, isModule, arrayDims + 1)
      case clazz       => ErasedTypeRef(name(qual, SimpleName(clazz), isModule).toTypeName, arrayDims)
    }

    var isModule = false

    val classKind = tname.toTermName match {
      case ObjectName(classKind) =>
        isModule = true
        classKind
      case nonModule => nonModule
    }

    classKind match {
      case terminal: SimpleName => // unqualified in the <empty> package
        specialised(Empty, terminal.raw, isModule)
      case QualifiedName(path, PathSep, terminal) =>
        specialised(path, terminal.raw, isModule)
      case _ =>
        throw new AssertionError(s"Unexpected erased type ref ${tname.debug}")
    }

  }

}
