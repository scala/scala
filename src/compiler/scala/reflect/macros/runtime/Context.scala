package scala.reflect.macros
package runtime

import scala.tools.nsc.Global

abstract class Context extends scala.reflect.macros.Context
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
                         with Synthetics
                         with Traces {

  val universe: Global

  val mirror: universe.Mirror = universe.rootMirror

  val callsiteTyper: universe.analyzer.Typer

  val prefix: Expr[PrefixType]

  val expandee: Tree
}
