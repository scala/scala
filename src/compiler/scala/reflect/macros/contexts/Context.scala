/* NSC -- new Scala compiler
 * Copyright 2013-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect.macros
package contexts

import scala.tools.nsc.Global

abstract class Context extends scala.reflect.macros.blackbox.Context
                          with scala.reflect.macros.whitebox.Context
                          with Aliases
                          with Enclosures
                          with Names
                          with Reifiers
                          with FrontEnds
                          with Infrastructure
                          with Typers
                          with Parsers
                          with Evals
                          with ExprUtils
                          with Traces
                          with Internals {

  val universe: Global

  val mirror: universe.Mirror = universe.rootMirror

  val callsiteTyper: universe.analyzer.Typer

  val prefix: Expr[PrefixType]

  val expandee: Tree
}
