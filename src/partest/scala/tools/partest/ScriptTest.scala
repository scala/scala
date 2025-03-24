/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.partest

import scala.reflect.internal.util.ScalaClassLoader

/** A `ScriptTest` is a `DirectTest` for which the code
 *  is the contents of a script file.
 */
abstract class ScriptTest extends DirectTest {
  def testmain = "TestMain"
  override def extraSettings = s"-usejavacp -Xscript $testmain"
  def scriptPath = testPath changeExtension "script"
  def code = scriptPath.toFile.slurp()
  def argv = Seq.empty[String]
  def show() = {
    assert(compile())
    ScalaClassLoader(getClass.getClassLoader).run(testmain, argv)
  }
}
