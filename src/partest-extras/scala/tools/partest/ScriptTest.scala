/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
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
  def code = scriptPath.toFile.slurp
  def argv = Seq.empty[String]
  def show() = {
    compile()
    ScalaClassLoader(getClass.getClassLoader).run(testmain, argv)
  }
}
