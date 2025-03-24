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

package scala.tools.nsc
package doc

import scala.tools.nsc.backend.JavaPlatform
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.typechecker.MacroAnnotationNamers

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
    override val initial = true
  }

  override lazy val loaders = new {
    val global: outer.type = outer
    val platform: outer.platform.type = outer.platform
  } with GlobalSymbolLoaders {
    // scala/bug#5593 Scaladoc's current strategy is to visit all packages in search of user code that can be documented
    // therefore, it will rummage through the classpath triggering errors whenever it encounters package objects
    // that are not in their correct place (see bug for details)
    override protected def signalError(root: Symbol, ex: Throwable): Unit =
      log(s"Suppressing error involving $root: $ex")
  }
}

// takes a `Reporter`, not `FilteringReporter` for sbt compatibility
class ScaladocGlobal(settings: doc.Settings, reporter: Reporter) extends Global(settings, reporter) with ScaladocGlobalTrait {
  self =>
  override protected def computeInternalPhases(): Unit = {
    phasesSet += syntaxAnalyzer
    phasesSet += analyzer.namerFactory
    phasesSet += analyzer.packageObjects
    phasesSet += analyzer.typerFactory
    phasesSet += terminal
  }

  override lazy val platform: ThisPlatform =
    new JavaPlatform {
      lazy val global: self.type = self
      override def platformPhases = Nil // used by computePlatformPhases
    }

  override def createJavadoc = if (settings.docNoJavaComments.value) false else true

  override lazy val analyzer =
    if (settings.YmacroAnnotations.value) new { val global: ScaladocGlobal.this.type = ScaladocGlobal.this } with ScaladocAnalyzer with MacroAnnotationNamers
    else new { val global: ScaladocGlobal.this.type = ScaladocGlobal.this } with ScaladocAnalyzer
}
