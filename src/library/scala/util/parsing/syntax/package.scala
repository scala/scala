/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.util.parsing

import scala.util.parsing.combinator.token

/** If deprecating the whole package worked, that's what would best
 *  be done, but it doesn't (yet) so it isn't.
 */
package object syntax {
  @deprecated("Moved to scala.util.parsing.combinator.token", "2.8.0")
  type Tokens = token.Tokens
  @deprecated("Moved to scala.util.parsing.combinator.token", "2.8.0")
  type StdTokens = token.StdTokens
}
