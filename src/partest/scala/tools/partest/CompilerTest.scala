/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.partest

import scala.tools.nsc._

/** For testing compiler internals directly.
 *  Each source code string in "sources" will be compiled, and
 *  the check function will be called with the source code and the
 *  resulting CompilationUnit.  The check implementation should
 *  test for what it wants to test and fail (via assert or other
 *  exception) if it is not happy.
 */
abstract class CompilerTest extends DirectTest {
  def check(source: String, unit: global.CompilationUnit): Unit

  lazy val global: Global = newCompiler()
  lazy val units = compilationUnits(global)(sources: _ *)
  
  override def extraSettings = "-usejavacp -d " + testOutput.path

  def sources: List[String] = List(code)
  def show() = (sources, units).zipped foreach check
}
