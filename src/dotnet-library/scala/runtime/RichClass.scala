/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime

import Predef.Class

final class RichClass(val self: Class) extends Proxy {

  def isPrimitive(): Boolean = self.IsPrimitive
  def isArray(): Boolean = self.IsArray

  def getClass(): RichClass = this
  def getName(): String = self.Name
  def getComponentType(): Class = self.GetElementType

}
