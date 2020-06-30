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
  // The defined classes in the test (internal names)
  val definedClasses: Seq[String]

  def loadSourceDebugExtensionAttribute(fqcn: String): (String, String) = {
    val cn = loadClassNode(fqcn, skipDebugInfo = false)
    (cn.sourceFile, cn.sourceDebug)
  }

  override def show(): Unit = {
    for (cls <- definedClasses) {
      val (sourceFile, sourceDebug) = loadSourceDebugExtensionAttribute(cls)
      println(s"""SourceFile: "${sourceFile}"""")
      println(s"SourceDebugExtension:\n$sourceDebug")
    }
  }
}
