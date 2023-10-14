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

import scala.language.implicitConversions
import scala.reflect.runtime.{universe => ru}
import scala.tools.nsc._

/** A DirectTest for testing compiler internals.
 *  The test must implement the `check` function to check
 *  the result of compiling the `code`; the test may override
 *  `sources` instead to check multiple sources.
 *
 *  Each source code string in "sources" will be compiled, and
 *  the check function will be called with the source code and the
 *  resulting CompilationUnit.  The check implementation should
 *  test for what it wants to test and fail (via assert or other
 *  exception) if it is not happy.
 */
abstract class CompilerTest extends DirectTest {
  def check(source: String, unit: global.CompilationUnit): Unit

  lazy val global: Global = newCompiler()
  lazy val units: List[global.CompilationUnit] = compilationUnits(global)(sources: _ *)
  import global._
  import definitions.compilerTypeFromTag

  def show() = sources.lazyZip(units).foreach(check)

  // Override at least one of these...
  def code = ""
  def sources: List[String] = List(code)

  // Utility functions
  class MkType(sym: Symbol) {
    def apply[M](implicit t: ru.TypeTag[M]): Type =
      if (sym eq NoSymbol) NoType
      else appliedType(sym, compilerTypeFromTag(t))
  }
  implicit def mkMkType(sym: Symbol): MkType = new MkType(sym)

  def allMembers(root: Symbol): List[Symbol] = {
    def loop(seen: Set[Symbol], roots: List[Symbol]): List[Symbol] = {
      val latest = roots flatMap (_.info.members) filterNot (seen contains _)
      if (latest.isEmpty) seen.toList.sortWith(_ isLess _)
      else loop(seen ++ latest, latest)
    }
    loop(Set(), List(root))
  }

  class SymsInPackage(pkgName: String) {
    def pkg     = rootMirror.getPackage(pkgName)
    def classes = allMembers(pkg) filter (_.isClass)
    def modules = allMembers(pkg) filter (_.isModule)
    def symbols = classes ++ terms filterNot (_ eq NoSymbol)
    def terms   = allMembers(pkg) filter (s => s.isTerm && !s.isConstructor)
    def tparams = classes flatMap (_.info.typeParams)
    def tpes    = symbols.map(_.tpe).distinct
  }
}
