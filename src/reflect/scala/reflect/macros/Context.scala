package scala.reflect
package macros

// todo. introduce context hierarchy
// the most lightweight context should just expose the stuff from the SIP
// the full context should include all traits from scala.reflect.macros (and probably reside in scala-compiler.jar)

trait Context extends Aliases
                 with CapturedVariables
                 with Enclosures
                 with Infrastructure
                 with Names
                 with Reifiers
                 with FrontEnds
                 with Settings
                 with Typers
                 with Parsers
                 with Exprs
                 with TypeTags
                 with Evals
                 with ExprUtils {

  /** The compile-time universe */
  val universe: Universe

  /** The mirror of the compile-time universe */
  val mirror: MirrorOf[universe.type]

  /** The type of the prefix tree from which the macro is selected */
  type PrefixType

  /** The prefix tree from which the macro is selected */
  val prefix: Expr[PrefixType]
}
