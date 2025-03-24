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
                            val allowExceptionsToPropagate: Boolean,
                            val typingTransformer: TypingTransformer,
                            val exteralFsmSelfParam: Symbol,
                            val applyTrParam: Symbol,
                            val asyncType: Type,
                            val asyncNames: AsyncNames[global.type]) {
    val localTyper: analyzer.Typer = typingTransformer.localTyper
    val stateAssigner = new StateAssigner
    val labelDefStates = collection.mutable.Map[Symbol, Int]()
    val name = new asyncNames.AsyncName(localTyper.context.unit.fresh)

    lazy val Async_await: Symbol = awaitSymbol

    val applyTr: Symbol = applyTrParam
    val applySym: Symbol = applyTr.owner
    var currentPos: Position = applySym.pos

    lazy val stateMachineClass: Symbol = if (exteralFsmSelfParam != NoSymbol) exteralFsmSelfParam.info.typeSymbol else applySym.owner
    lazy val stateGetter: Symbol = stateMachineMember(nme.state)
    lazy val stateSetter: Symbol = stateMachineMember(nme.state.setterName)
    lazy val stateOnComplete: Symbol = stateMachineMember(TermName("onComplete"))
    lazy val stateCompleteSuccess: Symbol = stateMachineMember(TermName("completeSuccess"))
    lazy val stateCompleteFailure: Symbol = stateMachineMember(TermName("completeFailure"))
    lazy val stateGetCompleted: Symbol = stateMachineMember(TermName("getCompleted"))
    lazy val stateTryGet: Symbol = stateMachineMember(TermName("tryGet"))
    lazy val whileLabel: Symbol = applySym.newLabel(TermName(nme.WHILE_PREFIX)).setInfo(MethodType(Nil, definitions.UnitTpe))

    lazy val tryGetIsIdentity: Boolean = exitingTyper {
      stateTryGet.info.finalResultType.termSymbol == stateTryGet.firstParam
    }
    def stateMachineMember(name: TermName): Symbol =
      stateMachineClass.info.member(name)
    def memberRef(sym: Symbol): Tree =
      if (exteralFsmSelfParam == NoSymbol)
        gen.mkAttributedRef(stateMachineClass.typeConstructor, sym)
      else
        gen.mkAttributedSelect(gen.mkAttributedIdent(exteralFsmSelfParam), sym)
    def stateMachineRef(): Tree =
      if (exteralFsmSelfParam == NoSymbol)
        gen.mkAttributedThis(stateMachineClass)
      else
        gen.mkAttributedIdent(exteralFsmSelfParam)
  }

}