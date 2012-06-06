package scala.reflect.makro

import language.experimental.macros

// todo. introduce context hierarchy
// the most lightweight context should just expose the stuff from the SIP
// the full context should include all traits from scala.reflect.makro (and probably reside in scala-compiler.jar)

trait Context extends Aliases
                 with CapturedVariables
                 with Enclosures
                 with Infrastructure
                 with Names
                 with Reifiers
                 with FrontEnds
                 with Settings
                 with Symbols
                 with Typers
                 with Util {

  /** The mirror that corresponds to the compile-time universe */
  val mirror: scala.reflect.api.Universe

  /** The type of the prefix tree from which the macro is selected */
  type PrefixType

  /** The prefix tree from which the macro is selected */
  val prefix: Expr[PrefixType]

  /** Alias to the underlying mirror's reify */
  // implementation is magically hardwired to `scala.reflect.reify.Taggers`
  def reify[T](expr: T): Expr[T] = macro ???
}
