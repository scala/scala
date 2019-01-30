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

import user.{AsyncBase, FutureSystem}
import scala.reflect.internal.Flags

// TODO: check there's no await outside of an async block

abstract class AsyncEarlyExpansion extends AsyncContext {
  import u._

  // NOTE: this part runs during typer
  lazy val futureSystem: FutureSystem = asyncBase.futureSystem
  lazy val futureSystemOps: futureSystem.Ops[u.type] = futureSystem.mkOps(u, false)

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
        class stateMachine$async extends scala.runtime.AbstractFunction1[scala.util.Try[`resultType`],Unit] with () => Unit {
          def <init>(): stateMachine$async = {
            stateMachine$async.super.<init>();
            ()
          };
          private[this] var state$async: Int = 0;
          private[this] val result$async: scala.concurrent.Promise[`resultType`] = Promise.apply[`resultType`]();
          <stable> <accessor> def result$async: scala.concurrent.Promise[`resultType`] = stateMachine$async.this.result$async;
          private[this] val execContext$async: scala.concurrent.ExecutionContext = `execContext`;
          <stable> <accessor> def execContext$async: scala.concurrent.ExecutionContext = stateMachine$async.this.execContext$async;
          def apply(): Unit = stateMachine$async.this.apply(null);
          def apply(tr$async: scala.util.Try[`resultType`]): Unit = {
            scala.async.async[`resultType`](`asyncBody`)(`execContext`);
            ()
          }
        };
        val stateMachine$async: stateMachine$async = new stateMachine$async();
        Future.apply[stateMachine$async](stateMachine$async)(stateMachine$async.execContext$async);
        stateMachine$async.result$async.future
      }
    */
  def apply(asyncBody: Tree, execContext: Tree, resultType: Type, originalOwner: Symbol) = {
    val tryResult = futureSystemOps.tryType(resultType)

    val stateMachine: ClassDef = {
      val parents = {
        val customParents = futureSystemOps.stateMachineClassParents
        // prefer extending a class to reduce the class file size of the state machine.
        // ... unless a custom future system already extends some class
        val useClass = customParents.forall(_.typeSymbol.asClass.isTrait)

        val fun1Tpe =
          if (useClass) definitions.abstractFunctionType(tryResult :: Nil, definitions.UnitTpe)
          else definitions.functionType(tryResult :: Nil, definitions.UnitTpe)

        // We extend () => Unit so we can pass this class as the by-name argument to `Future.apply`.
        // See SI-1247 for the the optimization that avoids creation.
        val funParents = List(fun1Tpe, definitions.functionType(Nil, definitions.UnitTpe))
        (customParents ::: funParents).map(TypeTree(_))
      }

      val stateVar =
        ValDef(Modifiers(Flags.MUTABLE | Flags.PRIVATE | Flags.LOCAL), nme.state, TypeTree(definitions.IntTpe), Literal(Constant(StateAssigner.Initial)))

      val resultVal =
        ValDef(NoMods, nme.result, TypeTree(futureSystemOps.promType(resultType)), futureSystemOps.createProm(resultType))

      val execContextVal =
        ValDef(NoMods, nme.execContext, TypeTree(execContext.tpe), execContext)

      val apply0Def =
        DefDef(NoMods, nme.apply, Nil, List(Nil), TypeTree(definitions.UnitTpe), Apply(Ident(nme.apply), Literal(Constant(null)) :: Nil))

      val applyFSM: DefDef = {
        val applyVParamss = List(List(ValDef(Modifiers(Flags.PARAM), nme.tr, TypeTree(tryResult), EmptyTree)))
        DefDef(NoMods, nme.apply, Nil, applyVParamss, TypeTree(definitions.UnitTpe), asyncBody).updateAttachment(ChangeOwnerAttachment(originalOwner))
      }

      atPos(asyncBody.pos)(ClassDef(NoMods, tpnme.stateMachine, Nil,
                                     gen.mkTemplate(parents, noSelfType, NoMods, List(Nil),
                                                     List(stateVar, resultVal, execContextVal, apply0Def, applyFSM))))
    }

    val newStateMachine = ValDef(NoMods, nme.stateMachine, TypeTree(), Apply(Select(New(Ident(tpnme.stateMachine)), nme.CONSTRUCTOR), Nil))
    // Note the invocation `.apply()` in `scala.concurrent.Future.apply[$resultType](stateMachine$async.apply())(stateMachine$async.execContext$async);`
    // so that, after uncurry, we get: scala.concurrent.Future.apply[$resultType](stateMachine$async, stateMachine$async.execContext$async());
    val stateMachineToFuture = futureSystemOps.future(Apply(Select(Ident(nme.stateMachine), nme.apply), Nil), Select(Ident(nme.stateMachine), nme.execContext))
    val promToFuture = futureSystemOps.promiseToFuture(Select(Ident(nme.stateMachine), nme.result))

    Block(List(stateMachine, newStateMachine, stateMachineToFuture), promToFuture)
  }
}

// This was originally a macro -- TODO: complete integration with compiler universe (use global instead of scala.reflect.internal stuff)
abstract class AsyncTransform(val asyncBase: AsyncBase) extends AnfTransform with AsyncAnalysis with Lifter with LiveVariables {
  import u._
  import typingTransformers.{TypingTransformApi, typingTransform}

  // synthesize the state machine logic -- explode the apply method's rhs and lift local vals to field defs in the state machine
  /*
      class stateMachine$async extends scala.runtime.AbstractFunction1 with Function0$mcV$sp {
        def <init>(): stateMachine$async = {
          stateMachine$async.super.<init>();
          stateMachine$async.super./*Function0*/$init$();
          ()
        };
        private[this] var state$async: Int = 0;
        private[this] val result$async: scala.concurrent.Promise = Promise.apply();
        <stable> <accessor> def result$async(): scala.concurrent.Promise = stateMachine$async.this.result$async;
        private[this] val execContext$async: scala.concurrent.ExecutionContext = `execContext`;
        <stable> <accessor> def execContext$async(): scala.concurrent.ExecutionContext = stateMachine$async.this.execContext$async;
        def apply(): Unit = stateMachine$async.this.apply$mcV$sp();
        def apply(tr$async: scala.util.Try): Unit = { // symbol of this def is `applySym`, symbol of its param named "tr$async" is `trParamSym`
          scala.async.async(`asyncBody`, `execContext`);
          ()
        };
        <specialized> def apply$mcV$sp(): Unit = stateMachine$async.this.apply(null);
        <bridge> <artifact> def apply(v1: Object): Object = {
          stateMachine$async.this.apply(v1.$asInstanceOf[scala.util.Try]());
          scala.runtime.BoxedUnit.UNIT
        };
        <bridge> <artifact> def apply(): Object = {
          stateMachine$async.this.apply();
          scala.runtime.BoxedUnit.UNIT
        }
      };
   */
  def asyncTransform(asyncBody: Tree, applySym: Symbol, trParamSym: Symbol, execContext: Tree): Option[(Tree, List[Tree])] = {
    val asyncPos = asyncBody.pos
    val stateMachineClass = applySym.owner
    val resultType = exitingTyper { futureSystemOps.tryTypeToResult(trParamSym.info) }

    markContainsAwait(asyncBody) // ANF transform also relies on whether something contains await
    reportUnsupportedAwaits(asyncBody)

    // Transform to A-normal form:
    //  - no await calls in qualifiers or arguments,
    //  - if/match only used in statement position.
    val anfTree0: Block = anfTransform(asyncBody, applySym)

    val anfTree = futureSystemOps.postAnfTransform(anfTree0)

    // TODO: why redo this?
    cleanupContainsAwaitAttachments(anfTree)
    markContainsAwait(anfTree)

    val asyncBlock = buildAsyncBlock(anfTree, SymLookup(stateMachineClass, trParamSym))

    // generate lean code for the simple case of `async { 1 + 1 }`
    if (asyncBlock.asyncStates.lengthCompare(1) == 0) None
    else {
      val liftedFields: List[Tree] = liftables(asyncBlock.asyncStates)

      // live variables analysis
      // the result map indicates in which states a given field should be nulled out
      val assignsOf = fieldsToNullOut(asyncBlock.asyncStates, liftedFields)

      for ((state, flds) <- assignsOf) {
        val assigns = flds.map { fld =>
          val fieldSym = fld.symbol
          val assign = Assign(gen.mkAttributedStableRef(thisType(fieldSym.owner), fieldSym), mkZero(fieldSym.info, asyncPos))
          val nulled = nullOut(fieldSym)
          if (isLiteralUnit(nulled)) assign
          else Block(nulled :: Nil, assign)
        }
        val asyncState = asyncBlock.asyncStates.find(_.state == state).get
        asyncState.stats = assigns ++ asyncState.stats
      }

      val liftedSyms = liftedFields.map(_.symbol).toSet
      liftedSyms.foreach { sym =>
        if (sym != null) {
          sym.owner = stateMachineClass
          if (sym.isModule)
            sym.asModule.moduleClass.owner = stateMachineClass
        }
      }

      // Replace the ValDefs in the async block with Assigns to the corresponding lifted
      // fields. Similarly, replace references to them with references to the field.
      val useFields: (Tree, TypingTransformApi) => Tree = (tree, api) => {
        def fieldSel =
          atPos(tree.pos)(Select(This(stateMachineClass).setType(stateMachineClass.tpe), tree.symbol).setType(tree.symbol.tpe))
        tree match {
          case ValDef(_, _, _, rhs) if liftedSyms(tree.symbol) =>
            if (tree.symbol.asTerm.isLazy) literalUnit
            else assignUnitType(treeCopy.Assign(tree, fieldSel, api.recur(rhs.changeOwner((tree.symbol, api.currentOwner)))))
          case _: DefTree if liftedSyms(tree.symbol)           => EmptyTree
          case Ident(name) if liftedSyms(tree.symbol)          => fieldSel.setType(tree.tpe)
          case _                                               => api.default(tree)
        }
      }

      val liftablesUseFields = liftedFields.map {
        case vd: ValDef if !vd.symbol.asTerm.isLazy => vd
        case x                                      => typingTransform(x, stateMachineClass)(useFields)
      }

      liftablesUseFields.foreach { t =>
        if (t.symbol != null) {
          stateMachineClass.info.decls.enter(t.symbol)
          // TODO AM: refine the resetting of the lazy flag -- this is so that local lazy vals that are lifted to the class
          // actually get their LazyRef allocated to the var that holds the lazy val's reference
          t.symbol.resetFlag(Flags.LAZY)
        }
      }

      val applyBody = atPos(asyncPos)(asyncBlock.onCompleteHandler(WeakTypeTag(transformType(resultType))))
      val applyRhs = typingTransform(applyBody, stateMachineClass)(useFields)
      if (AsyncUtils.verbose) {
        val location = try asyncBody.pos.source.path catch {
          case _: UnsupportedOperationException => asyncBody.pos.toString
        }
        logDiagnostics(location, anfTree, asyncBlock, asyncBlock.asyncStates.map(_.toString))
      }
      futureSystemOps.dot(applySym, asyncBody).foreach(f => f(asyncBlock.toDot))

      Some((cleanupContainsAwaitAttachments(applyRhs), liftablesUseFields))
    }
  }

  def logDiagnostics(location: String, anfTree: Tree, block: AsyncBlock, states: Seq[String]): Unit = {
    AsyncUtils.vprintln(s"In file '$location':")
    AsyncUtils.vprintln(s"ANF transform expands to:\n $anfTree")
    states foreach (s => AsyncUtils.vprintln(s))
    AsyncUtils.vprintln("===== DOT =====")
    AsyncUtils.vprintln(block.toDot)
  }
}
