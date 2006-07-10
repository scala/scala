/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: StringAdd.scala 7771 2006-06-12 13:22:39Z dubochet $
package scala.runtime

final class StringAdd(self: Any) {
  def +(other: String) = self.toString + other
}
