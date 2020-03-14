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

import scala.collection.mutable
import user.{FutureSystem, ScalaConcurrentFutureSystem}
import scala.reflect.internal.Flags
import scala.tools.nsc.Global
import scala.tools.nsc.transform.TypingTransformers

// NOTE: this part runs during typer to wrap argument to the `async` macro
// in a class. This will later serve as storage for state of the state
// machine, and before that will force the compiler to use outer pointers
// for references from the async block to members of its owning or other
// enclosing classes.
abstract class AsyncEarlyExpansion extends TypingTransformers {
  import global._

  private lazy val Promise_class = rootMirror.requiredClass[scala.concurrent.Promise[_]]

  /** Perform async macro expansion during typers to a block that creates the state machine class,
    * along with supporting definitions, but without the ANF/Async expansion.
    *
    * The full expansion of the actual state machine logic (anf & async) is performed by asyncTransform after erasure.
    * Until then, the state machine's apply method just has the original async macro invocation.
    *
    * The goal is to balance compiler performance by delaying tree explosion (due to anf and state machine mechanics) and
    * complexity arising from deftree synthesis in later phases, which would require
    * retro-actively running erasure (bridges/types) and explicitouter (add outer arg to state machine ctor)
    * on the synthetic def trees.
    *
    * Synthesizes:
      {
        val execContext0$async: scala.concurrent.ExecutionContext = `execContext`;
        class stateMachine$async extends extends scala.runtime.AbstractFunction1[scala.util.Try[Int],Unit] {
          def <init>(): stateMachine$async = {
            stateMachine$async.super.<init>();
            ()
          };
          private[this] var state$async: Int = 0;
          <accessor> private def state$async: Int = stateMachine$async.this.state$async;
          <accessor> private def state$async_=(x$1: Int): Unit = stateMachine$async.this.state$async = x$1;
          private[this] val result$async: scala.concurrent.Promise[`resultType`] = Promise.apply[`resultType`]();
          <stable> <accessor> def result$async: scala.concurrent.Promise[`resultType`] = stateMachine$async.this.result$async;
          private[this] val execContext$async: scala.concurrent.ExecutionContext = execContext0$async;
          <stable> <accessor> def execContext$async: scala.concurrent.ExecutionContext = stateMachine$async.this.execContext$async;
          def apply(tr$async: scala.util.Try[`resultType`]): Unit = {
            `asyncBody`
            ()
          }
        };
        val stateMachine$async: stateMachine$async = new stateMachine$async();
        scala.concurrent.Future.unit.onComplete[Unit](stateMachine$async.asInstanceOf[scala.util.Try[Unit] => Unit])(stateMachine$async.execContext$async);
        stateMachine$async.result$async.future
      }
    */
  def apply(callsiteTyper: analyzer.Typer, asyncBody: Tree, execContext: Tree, resultType: Type) = {
    val futureSystem: FutureSystem = ScalaConcurrentFutureSystem
    val futureSystemOps: futureSystem.Ops[global.type] = futureSystem.mkOps(global)

    val tryResult = futureSystemOps.tryType(resultType)

    val execContextTempVal =
      ValDef(NoMods, nme.execContextTemp, TypeTree(execContext.tpe), execContext)

    val stateMachine: ClassDef = {
      val parents = {
        val customParents = futureSystemOps.stateMachineClassParents
        // prefer extending a class to reduce the class file size of the state machine.
        // ... unless a custom future system already extends some class
        val useClass = customParents.forall(_.typeSymbol.isTrait)

        val fun1Tpe =
          if (useClass) definitions.abstractFunctionType(tryResult :: Nil, definitions.UnitTpe)
          else definitions.functionType(tryResult :: Nil, definitions.UnitTpe)

        val funParents = List(fun1Tpe)
        (customParents ::: funParents).map(TypeTree(_))
      }

      val stateVar =
        ValDef(Modifiers(Flags.MUTABLE | Flags.PRIVATE), nme.state, TypeTree(definitions.IntTpe), Literal(Constant(StateAssigner.Initial)))

      def createProm(resultType: Type): Tree =
        Apply(TypeApply(gen.mkAttributedStableRef(Promise_class.companionModule), TypeTree(resultType) :: Nil), Nil)

      val resultVal =
        ValDef(NoMods, nme.result, TypeTree(appliedType(Promise_class, resultType)), createProm(resultType))

      val execContextVal =
        ValDef(NoMods, nme.execContext, TypeTree(execContext.tpe), Ident(nme.execContextTemp))

      val applyFSM: DefDef = {
        val applyVParamss = List(List(ValDef(Modifiers(Flags.PARAM), nme.tr, TypeTree(tryResult), EmptyTree)))
        DefDef(NoMods, nme.apply, Nil, applyVParamss, TypeTree(definitions.UnitTpe), Block(
          asyncBody.updateAttachment(SuppressPureExpressionWarning), Literal(Constant(())))
        ).updateAttachment(ChangeOwnerAttachment(callsiteTyper.context.owner))
      }
      async.addFutureSystemAttachment(callsiteTyper.context.unit, applyFSM, futureSystem)

      atPos(asyncBody.pos)(ClassDef(NoMods, tpnme.stateMachine, Nil,
                                     gen.mkTemplate(parents, noSelfType, NoMods, List(Nil),
                                                     List(stateVar, resultVal, execContextVal, applyFSM))))
    }

    val newStateMachine = ValDef(NoMods, nme.stateMachine, TypeTree(), Apply(Select(New(Ident(tpnme.stateMachine)), nme.CONSTRUCTOR), Nil))
    def execContextSelect = Select(Ident(nme.stateMachine), nme.execContext)

    // Use KeptPromise.onComplete to get the ball rolling.
    val futureUnit = futureSystemOps.futureUnit(execContextSelect)

    // stateMachine.asInstanceOf[Function1[Try[Unit], Unit]
    // This cast is safe because we know that `def apply` does not consult its argument when `state == 0`.
    val castStateMachine = gen.mkCast(Ident(nme.stateMachine),
      definitions.functionType(futureSystemOps.tryType(definitions.UnitTpe) :: Nil, definitions.UnitTpe))

    val stateMachineToFuture = futureSystemOps.onComplete(futureUnit, castStateMachine, execContextSelect)

    val promToFuture = Select(Select(Ident(nme.stateMachine), nme.result), nme.future)

    Block(List(execContextTempVal, stateMachine, newStateMachine, stateMachineToFuture), promToFuture)
  }
}
