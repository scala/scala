/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package doc

import reporters.Reporter

trait ScaladocGlobalTrait extends Global {
  outer =>

  override val useOffsetPositions = false
  override def newUnitParser(unit: CompilationUnit) = new syntaxAnalyzer.ScaladocUnitParser(unit, Nil)
  override def newJavaUnitParser(unit: CompilationUnit) = if (createJavadoc) {
    new syntaxAnalyzer.ScaladocJavaUnitParser(unit)
  } else {
    super.newJavaUnitParser(unit)
  }

  override lazy val syntaxAnalyzer = new ScaladocSyntaxAnalyzer[outer.type](outer) {
    val runsAfter = List[String]()
    val runsRightAfter = None
  }

  override lazy val loaders = new {
    val global: outer.type = outer
    val platform: outer.platform.type = outer.platform
  } with GlobalSymbolLoaders {
    // SI-5593 Scaladoc's current strategy is to visit all packages in search of user code that can be documented
    // therefore, it will rummage through the classpath triggering errors whenever it encounters package objects
    // that are not in their correct place (see bug for details)
    override protected def signalError(root: Symbol, ex: Throwable) {
      log(s"Suppressing error involving $root: $ex")
    }
  }
}

class ScaladocGlobal(settings: doc.Settings, reporter: Reporter) extends Global(settings, reporter) with ScaladocGlobalTrait {
  override protected def computeInternalPhases() {
    phasesSet += syntaxAnalyzer
    phasesSet += analyzer.namerFactory
    phasesSet += analyzer.packageObjects
    phasesSet += analyzer.typerFactory
  }
  override def forScaladoc = true
  override def createJavadoc = if (settings.docNoJavaComments.value) false else true

  override lazy val analyzer = new {
    val global: ScaladocGlobal.this.type = ScaladocGlobal.this
  } with ScaladocAnalyzer
}
