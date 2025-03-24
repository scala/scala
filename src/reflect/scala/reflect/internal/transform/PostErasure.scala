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

package scala.reflect
package internal
package transform

trait PostErasure {
  // FIXME: With `global` as a `val`, implementers must use early initializers, which
  //        are deprecated and will not be supported in 3.0. Please change the design,
  //        remove the early initializers from implementers, and then remove the
  //        `@nowarn` annotations from implementers.
  val global: SymbolTable
  import global._

  object elimErasedValueType extends TypeMap {
    def apply(tp: Type) = tp match {
      case ConstantType(Constant(tp: Type)) => ConstantType(Constant(apply(tp)))
      case ErasedValueType(_, underlying)   => underlying
      case null                             => null
      case _                                => tp.mapOver(this)
    }
  }

  def transformInfo(sym: Symbol, tp: Type) = elimErasedValueType(tp)
}
