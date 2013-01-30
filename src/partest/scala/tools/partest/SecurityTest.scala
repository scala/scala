/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.partest

import java.security._
import java.util._

abstract class SecurityTest extends App {
  def throwIt(x: Any) = throw new AccessControlException("" + x)
  def propertyCheck(p: PropertyPermission): Unit = throwIt(p)

  def check(perm: Permission): Unit = perm match {
    case p: PropertyPermission  => propertyCheck(p)
    case _                      => ()
  }
}
