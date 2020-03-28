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

package scala.tools.nsc.transform.async

import scala.tools.nsc.Global
import scala.tools.nsc.transform.TypingTransformers

trait AsyncTransformStates extends TypingTransformers {
  private[async] def currentTransformState: AsyncTransformState

  val global: Global
  import global._

  class AsyncTransformState(val awaitSymbol: Symbol,
                            val postAnfTransform: Block => Block,
                            val dotDiagram: (Symbol, Tree) => Option[String => Unit],
                            val typingTransformer: TypingTransformer,
                            val applyTrParam: Symbol,
                            val asyncType: Type,
                            val asyncNames: AsyncNames[global.type]) {
    val localTyper: analyzer.Typer = typingTransformer.localTyper
    val stateAssigner = new StateAssigner
    val labelDefStates = collection.mutable.Map[Symbol, Int]()
    object name extends asyncNames.AsyncName {
      def fresh(name: TermName): TermName = freshenIfNeeded(name)
    }

    lazy val Async_await: Symbol = awaitSymbol

    lazy val applyTr: Symbol = applyTrParam
    lazy val applySym: Symbol = applyTr.owner
    lazy val stateMachineClass: Symbol = applySym.owner
    lazy val stateGetter: Symbol = stateMachineMember(nme.state)
    lazy val stateSetter: Symbol = stateGetter.setterIn(stateGetter.owner)
    lazy val stateOnComplete: Symbol = stateMachineMember(TermName("onComplete"))
    lazy val stateCompleteSuccess: Symbol = stateMachineMember(TermName("completeSuccess"))
    lazy val stateCompleteFailure: Symbol = stateMachineMember(TermName("completeFailure"))
    lazy val stateGetCompleted: Symbol = stateMachineMember(TermName("getCompleted"))
    lazy val stateTryGet: Symbol = stateMachineMember(TermName("tryGet"))
    lazy val whileLabel: Symbol = applySym.newLabel(nme.WHILE_PREFIX).setInfo(MethodType(Nil, definitions.UnitTpe))

    def stateMachineMember(name: TermName): Symbol =
      stateMachineClass.info.member(name)
    def memberRef(sym: Symbol): Tree =
      gen.mkAttributedRef(stateMachineClass.typeConstructor, sym)
  }

}