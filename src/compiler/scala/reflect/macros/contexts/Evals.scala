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
package contexts

import scala.reflect.runtime.{universe => ru}
import scala.tools.reflect.ToolBox

trait Evals {
  self: Context =>

  private lazy val evalMirror = ru.runtimeMirror(universe.analyzer.defaultMacroClassloader)
  private lazy val evalToolBox = evalMirror.mkToolBox()
  private lazy val evalImporter = ru.internal.createImporter(universe).asInstanceOf[ru.Importer { val from: universe.type }]

  def eval[T](expr: Expr[T]): T = {
    def specialK(x: Any) =
      x match {
        case _: global.TypeRef => true
        case _: global.Symbol  => true
        case _                 => false
      }
    expr.tree match {
      case global.Literal(global.Constant(value)) if !specialK(value) =>
        value.asInstanceOf[T]
      case _ =>
        val imported = evalImporter.importTree(expr.tree)
        evalToolBox.eval(imported).asInstanceOf[T]
    }
  }
}
