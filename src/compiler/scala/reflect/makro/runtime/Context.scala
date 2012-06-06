package scala.reflect.makro
package runtime

import scala.tools.nsc.Global

abstract class Context extends scala.reflect.makro.Context
                         with Aliases
                         with CapturedVariables
                         with Infrastructure
                         with Enclosures
                         with Mirrors
                         with Names
                         with Reifiers
                         with FrontEnds
                         with Settings
                         with Typers
                         with Parsers
                         with Exprs
                         with TypeTags
                         with Evals
                         with ExprUtils
                         with Traces {

  val universe: Global

  val mirror: MirrorOf[universe.type] = new ContextMirror

  val callsiteTyper: universe.analyzer.Typer

  val prefix: Expr[PrefixType]

  val expandee: Tree
}
