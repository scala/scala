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

abstract class JSR45Test extends BytecodeTest {
  def loadSourceDebugExtensionAttribute(fqcn: String): String = {
    loadClassNode(fqcn).sourceDebug
  }
}
