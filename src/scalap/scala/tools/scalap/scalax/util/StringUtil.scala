/*
 * Scala classfile decoder (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.scalap
package scalax
package util

import java.beans.Introspector

/**
 * @author ilyas
 */

object StringUtil {

  def decapitalize(s: String) = Introspector.decapitalize(s)

  def cutSubstring(dom: String)(s: String) = if (dom != null && s != null) dom.replace(s, "") else dom

}
