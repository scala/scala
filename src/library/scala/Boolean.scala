/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// DO NOT EDIT, CHANGES WILL BE LOST.

package scala

import java.{ lang => jl }

final class Boolean extends AnyVal {
  def unary_! : Boolean = sys.error("stub")

  def ==(x: Boolean): Boolean = sys.error("stub")
  def !=(x: Boolean): Boolean = sys.error("stub")
  def ||(x: Boolean): Boolean = sys.error("stub")
  def &&(x: Boolean): Boolean = sys.error("stub")
  // Compiler won't build with these seemingly more accurate signatures
  // def ||(x: => Boolean): Boolean = sys.error("stub")
  // def &&(x: => Boolean): Boolean = sys.error("stub")
  def |(x: Boolean): Boolean  = sys.error("stub")
  def &(x: Boolean): Boolean  = sys.error("stub")
  def ^(x: Boolean): Boolean  = sys.error("stub")
}

object Boolean extends AnyValCompanion {
  override def toString = "object scala.Boolean"
  def box(x: Boolean): jl.Boolean = jl.Boolean.valueOf(x)
  def unbox(x: jl.Object): Boolean = x.asInstanceOf[jl.Boolean].booleanValue()
}