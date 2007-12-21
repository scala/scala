/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime

import Predef.Class

final class RichClass[A](val self: Class[A]) extends Proxy {

  def isPrimitive(): Boolean = self.IsPrimitive
  def isArray(): Boolean = self.IsArray

  def getClass(): RichClass[A] = this
  def getName(): String = self.Name
  def getComponentType(): Class[A] = self.GetElementType

}
