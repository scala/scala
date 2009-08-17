/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.io

import collection.Traversable

object Path
{
  //
  // def canonicalize
  def apply(path: String) = new Path(path)
}
import Path._

// The path constructor is private so we can enforce that
// the value of `path' is always in its canonical form.
class Path private (val path: String)
{
}
