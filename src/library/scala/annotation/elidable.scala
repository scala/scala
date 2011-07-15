/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.annotation

import java.util.logging.Level

/** An annotation for methods for which invocations might
 *  be removed in the generated code.
 *
 *  Behavior is influenced by passing `-Xelide-below <arg>` to `scalac`.
 *  Methods marked elidable will be omitted from generated code if the
 *  priority given the annotation is lower than to the command line argument.
 *  Examples:
 *  {{{
 *  import annotation.elidable._
 *
 *    @elidable(WARNING) def foo = log("foo")
 *    @elidable(FINE) def bar = log("bar")
 *
 *  scalac -Xelide-below=1000
 *  }}}
 *  @since 2.8
 */
final class elidable(final val level: Int) extends annotation.StaticAnnotation {}

/** This useless appearing code was necessary to allow people to use
 *  named constants for the elidable annotation.  This is what it takes
 *  to convince the compiler to fold the constants: otherwise when it's
 *  time to check an elision level it's staring at a tree like
 *  {{{
 *  (Select(Level, Select(FINEST, Apply(intValue, Nil))))
 *  }}}
 *  instead of the number `300`.
 *
 *  @since 2.8
 */
object elidable {
  /** The levels `ALL` and `OFF` are confusing in this context because
   *  the sentiment being expressed when using the annotation is at cross
   *  purposes with the one being expressed via `-Xelide-below`.  This
   *  confusion reaches its zenith at level `OFF`, where the annotation means
   *  ''never elide this method'' but `-Xelide-below OFF` is how you would
   *  say ''elide everything possible''.
   *
   *  With no simple remedy at hand, the issue is now at least documented,
   *  and aliases `MAXIMUM` and `MINIMUM` are offered.
   */
  final val ALL     = Int.MinValue  // Level.ALL.intValue()
  final val FINEST  = 300           // Level.FINEST.intValue()
  final val FINER   = 400           // Level.FINER.intValue()
  final val FINE    = 500           // Level.FINE.intValue()
  final val CONFIG  = 700           // Level.CONFIG.intValue()
  final val INFO    = 800           // Level.INFO.intValue()
  final val WARNING = 900           // Level.WARNING.intValue()
  final val SEVERE  = 1000          // Level.SEVERE.intValue()
  final val OFF     = Int.MaxValue  // Level.OFF.intValue()

  // a couple aliases for the confusing ALL and OFF
  final val MAXIMUM = OFF
  final val MINIMUM = ALL

  // and we can add a few of our own
  final val ASSERTION = 2000    // we should make this more granular

  // for command line parsing so we can use names or ints
  val byName: Map[String, Int] = Map(
    "FINEST" -> FINEST,
    "FINER" -> FINER,
    "FINE" -> FINE,
    "CONFIG" -> CONFIG,
    "INFO" -> INFO,
    "WARNING" -> WARNING,
    "SEVERE" -> SEVERE,
    "ASSERTION" -> ASSERTION,
    "ALL" -> ALL,
    "OFF" -> OFF,
    "MAXIMUM" -> MAXIMUM,
    "MINIMUM" -> MINIMUM
  )
}
