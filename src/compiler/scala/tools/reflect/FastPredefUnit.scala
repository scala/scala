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

package scala.tools.reflect

import scala.reflect.macros.runtime.Context

trait FastPredefUnit {
  val c: Context
  val global: c.universe.type = c.universe

  import c.universe._
  import treeInfo.Applied

  def expandPredefUnit: Tree = c.macroApplication match {
    case Applied(_, _, List(List(arg))) =>
      q"""_root_.scala.Predef.locally { val _ = $arg }"""
    case other =>
      global.abort(s"Unexpected application ${showRaw(other)}")
      other
  }
}
