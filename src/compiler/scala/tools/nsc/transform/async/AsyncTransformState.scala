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

import user.FutureSystem
import scala.tools.nsc.Global
import scala.tools.nsc.transform.TypingTransformers

class AsyncTransformState[U <: Global with Singleton](val symbolTable: U, val futureSystem: FutureSystem,
                                                      val typingTransformer: TypingTransformers#TypingTransformer,
                                                      val applyTrParam: U#Symbol,
                                                      val asyncType: U#Type) {
  import symbolTable._
  val ops: futureSystem.Ops[symbolTable.type] = futureSystem.mkOps(symbolTable)

  val localTyper: symbolTable.analyzer.Typer = typingTransformer.localTyper.asInstanceOf[symbolTable.analyzer.Typer]
  val stateAssigner  = new StateAssigner
  val labelDefStates = collection.mutable.Map[symbolTable.Symbol, Int]()

  lazy val applyTr: Symbol = applyTrParam.asInstanceOf[symbolTable.Symbol]
  lazy val applySym: Symbol = applyTr.owner
  lazy val stateMachineClass: Symbol = applySym.owner
  lazy val stateGetter: Symbol = stateMachineMember(nme.state)
  lazy val stateSetter: Symbol = stateGetter.setterIn(stateGetter.owner)
  lazy val whileLabel: Symbol = applySym.newLabel(nme.WHILE_PREFIX).setInfo(MethodType(Nil, definitions.UnitTpe))

  def stateMachineMember(name: TermName): Symbol =
    stateMachineClass.info.member(name)
  def memberRef(name: TermName): Tree =
    gen.mkAttributedRef(stateMachineClass.typeConstructor, stateMachineMember(name))
  def memberRef(sym: Symbol): Tree =
    gen.mkAttributedRef(stateMachineClass.typeConstructor, sym)
  def selectResult: Tree = Apply(memberRef(nme.result), Nil)
}
