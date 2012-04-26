package scala.reflect.makro
package runtime

import scala.tools.nsc.Global

abstract class Context extends scala.reflect.makro.Context
                         with Aliases
                         with CapturedVariables
                         with Infrastructure
                         with Enclosures
                         with Names
                         with Reifiers
                         with FrontEnds
                         with Settings
                         with Symbols
                         with Typers
                         with Util
                         with Traces {

  val mirror: Global

  val callsiteTyper: mirror.analyzer.Typer

  val prefix: Expr[PrefixType]

  val expandee: Tree
}