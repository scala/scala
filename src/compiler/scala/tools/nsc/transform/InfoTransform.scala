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
package transform

import scala.annotation.nowarn

/**
 * An InfoTransform contains a compiler phase that transforms trees and symbol infos -- making sure they stay consistent.
 * The symbol info is transformed assuming it is consistent right before this phase.
 * The info transformation is triggered by Symbol::rawInfo, which caches the results in the symbol's type history.
 * This way sym.info (during an enteringPhase(p)) can look up what the symbol's info should look like at the beginning of phase p.
 * (If the transformed info had not been stored yet, rawInfo will compute the info by composing the info-transformers
 *  of the most recent phase before p, up to the transformer of the phase right before p.)
 *
 * Concretely, enteringPhase(p) { sym.info } yields the info *before* phase p has transformed it. Imagine you're a phase and it all makes sense.
 */
trait InfoTransform extends Transform {
  import global.{Symbol, Type, InfoTransformer, infoTransformers}

  def transformInfo(sym: Symbol, tpe: Type): Type

  override def newPhase(prev: scala.tools.nsc.Phase): StdPhase =
    new InfoPhase(prev)

  protected def changesBaseClasses = true
  protected def keepsTypeParams = true

  @nowarn("""cat=deprecation&origin=scala\.tools\.nsc\.transform\.InfoTransform\.Phase""")
  final type InfoPhase = Phase

  @nowarn("msg=shadowing a nested class of a parent is deprecated")
  @deprecated("use InfoPhase instead", since = "2.13.4")
  class Phase(prev: scala.tools.nsc.Phase) extends super.Phase(prev) {
    override val keepsTypeParams = InfoTransform.this.keepsTypeParams

    if (infoTransformers.nextFrom(id).pid != id) {
      // this phase is not yet in the infoTransformers
      val infoTransformer = new InfoTransformer {
        val pid = id
        val changesBaseClasses = InfoTransform.this.changesBaseClasses
        def transform(sym: Symbol, tpe: Type): Type = transformInfo(sym, tpe)
      }
      infoTransformers insert infoTransformer
    }
  }
}

