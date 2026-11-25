// Known limitation: looking up String completes the parents of Conv. Resolving `scala` in the
// Predef import goes through the wildcard import, which needs the parents of Conv.
object Conv extends (String => Int) { def apply(s: String) = 1; object x }

import Conv.x._
import scala.Predef.{assert}
