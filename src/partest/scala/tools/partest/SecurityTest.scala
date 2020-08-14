/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
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
