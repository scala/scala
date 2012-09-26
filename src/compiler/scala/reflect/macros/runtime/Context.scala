package scala.reflect.macros
package runtime

import scala.tools.nsc.Global

abstract class Context extends scala.reflect.macros.Context
                         with Aliases
                         with CapturedVariables
                         with Infrastructure
                         with Enclosures
                         with Names
                         with Reifiers
                         with FrontEnds
                         with Settings
                         with Typers
                         with Parsers
                         with Evals
                         with ExprUtils
                         with Traces {

  val universe: Global

  val mirror: scala.reflect.api.Mirror[universe.type] = universe.rootMirror

  val callsiteTyper: universe.analyzer.Typer

  val prefix: Expr[PrefixType]

  val expandee: Tree
}
