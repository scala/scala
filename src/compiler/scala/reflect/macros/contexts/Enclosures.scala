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

package scala.reflect.macros
package contexts

import scala.reflect.{ClassTag, classTag}

trait Enclosures {
  self: Context =>

  import universe._

  private lazy val site       = callsiteTyper.context

  private def lenientEnclosure[T <: Tree : ClassTag]: Tree = site.nextEnclosing(c => classTag[T].runtimeClass.isInstance(c.tree)).tree
  @deprecated("c.enclosingTree-style APIs are now deprecated; consult the scaladoc for more information", "2.13.4")
  private def strictEnclosure[T <: Tree : ClassTag]: T = site.nextEnclosing(c => classTag[T].runtimeClass.isInstance(c.tree)) match {
    case analyzer.NoContext => throw EnclosureException(classTag[T].runtimeClass, site.enclosingContextChain map (_.tree))
    case cx => cx.tree.asInstanceOf[T]
  }

  val macroApplication: Tree                      = expandee
  def enclosingPackage: PackageDef                = site.nextEnclosing(_.tree.isInstanceOf[PackageDef]).tree.asInstanceOf[PackageDef]
  lazy val enclosingClass: Tree                   = lenientEnclosure[ImplDef]
  @deprecated("c.enclosingTree-style APIs are now deprecated; consult the scaladoc for more information", "2.13.4")
  def enclosingImpl: ImplDef                      = strictEnclosure[ImplDef]
  @deprecated("c.enclosingTree-style APIs are now deprecated; consult the scaladoc for more information", "2.13.4")
  def enclosingTemplate: Template                 = strictEnclosure[Template]
  lazy val enclosingImplicits: List[ImplicitCandidate] = site.openImplicits.map(_.toImplicitCandidate)
  private val analyzerOpenMacros                  = universe.analyzer.openMacros
  val enclosingMacros: List[Context]              = this :: analyzerOpenMacros // include self
  lazy val enclosingMethod: Tree                       = lenientEnclosure[DefDef]
  @deprecated("c.enclosingTree-style APIs are now deprecated; consult the scaladoc for more information", "2.13.4")
  def enclosingDef: DefDef                        = strictEnclosure[DefDef]
  lazy val enclosingPosition: Position            = if (this.macroApplication.pos ne NoPosition) this.macroApplication.pos else {
    analyzerOpenMacros.collectFirst {
      case x if x.macroApplication.pos ne NoPosition => x.macroApplication.pos
    }.getOrElse(NoPosition)
  }
  @deprecated("c.enclosingTree-style APIs are now deprecated; consult the scaladoc for more information", "2.13.4")
  val enclosingUnit: CompilationUnit              = universe.currentRun.currentUnit
  @deprecated("c.enclosingTree-style APIs are now deprecated; consult the scaladoc for more information", "2.13.4")
  val enclosingRun: Run                           = universe.currentRun
}
