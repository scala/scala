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

import user.{FutureSystem, ScalaConcurrentFutureSystem}
import scala.reflect.internal.Flags
import scala.tools.nsc.Global
import scala.tools.nsc.transform.TypingTransformers

// TODO: check there's no await outside of an async block

abstract class AsyncEarlyExpansion extends TypingTransformers {
  import global._
  import global.async.FutureSystemAttachment

  // NOTE: this part runs during typer
  lazy val futureSystem: FutureSystem = ScalaConcurrentFutureSystem
  lazy val futureSystemOps: futureSystem.Ops[global.type] = futureSystem.mkOps(global)

  private lazy val Promise_class = rootMirror.requiredClass[scala.concurrent.Promise[_]]
  private def promType(tp: Type): Type = appliedType(Promise_class, tp)

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
  def apply(callsiteTyper: analyzer.Typer, asyncBody: Tree, execContext: Tree, resultType: Type, originalOwner: Symbol) = {
    val tryResult = futureSystemOps.tryType(resultType)

    val execContextTempVal =
      ValDef(NoMods, nme.execContextTemp, TypeTree(execContext.tpe), execContext)

    val stateMachine: ClassDef = {
      val parents = {
        val customParents = futureSystemOps.stateMachineClassParents
        // prefer extending a class to reduce the class file size of the state machine.
        // ... unless a custom future system already extends some class
        val useClass = customParents.forall(_.typeSymbol.asClass.isTrait)

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
        ValDef(NoMods, nme.result, TypeTree(promType(resultType)), createProm(resultType))

      val execContextVal =
        ValDef(NoMods, nme.execContext, TypeTree(execContext.tpe), Ident(nme.execContextTemp))

      val applyFSM: DefDef = {
        val applyVParamss = List(List(ValDef(Modifiers(Flags.PARAM), nme.tr, TypeTree(tryResult), EmptyTree)))
        DefDef(NoMods, nme.apply, Nil, applyVParamss, TypeTree(definitions.UnitTpe), Block(asyncBody.updateAttachment(SuppressPureExpressionWarning), Literal(Constant(())))).updateAttachment(ChangeOwnerAttachment(originalOwner))
      }

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

    Block(List(execContextTempVal, stateMachine, newStateMachine, stateMachineToFuture), promToFuture).updateAttachment(new FutureSystemAttachment(futureSystem))
  }
}

class AsyncTransformState[U <: Global with Singleton](val symbolTable: U, val futureSystem: FutureSystem,
                                                      val unit: U#CompilationUnit,
                                                      val typingTransformer: TypingTransformers#TypingTransformer) {
  val ops: futureSystem.Ops[symbolTable.type] = futureSystem.mkOps(symbolTable)
  val localTyper: symbolTable.analyzer.Typer = typingTransformer.localTyper.asInstanceOf[symbolTable.analyzer.Typer]
  val stateAssigner  = new StateAssigner
  val labelDefStates = collection.mutable.Map[symbolTable.Symbol, Int]()
}

// This was originally a macro -- TODO: complete integration with compiler universe (use global instead of scala.reflect.internal stuff)
trait AsyncTransform extends AnfTransform with AsyncAnalysis with Lifter with LiveVariables with TypingTransformers {
  var currentTransformState: AsyncTransformState[global.type] = null
  import global._
  def typecheck(tree: Tree): Tree = currentTransformState.localTyper.typed(tree)
  def abort(pos: Position, msg: String): Nothing = {currentTransformState.localTyper.context.reporter.error(pos, msg); ???}
  def error(pos: Position, msg: String): Unit = currentTransformState.localTyper.context.reporter.error(pos, msg)

  import global._

  // synthesize the state machine logic -- explode the apply method's rhs and lift local vals to field defs in the state machine
  /*
      val execContext0$async = `execContext`;
      class stateMachine$async extends scala.runtime.AbstractFunction1 {
        def <init>(): stateMachine$async = {
          stateMachine$async.super.<init>();
          stateMachine$async.super./*Function0*/$init$();
          ()
        };
        private[this] var state$async: Int = 0;
        <accessor> private def state$async(): Int = stateMachine$async.this.state$async;
        <accessor> private def state$async_=(x$1: Int): Unit = stateMachine$async.this.state$async = x$1;
        private[this] val result$async: scala.concurrent.Promise = Promise.apply();
        <stable> <accessor> def result$async(): scala.concurrent.Promise = stateMachine$async.this.result$async;
        private[this] val execContext$async: scala.concurrent.ExecutionContext = execContext0$async;
        <stable> <accessor> def execContext$async(): scala.concurrent.ExecutionContext = stateMachine$async.this.execContext$async;
        def apply(tr$async: scala.util.Try): Unit = { // symbol of this def is `applySym`, symbol of its param named "tr$async" is `trParamSym`
          `asyncBody`
          ()
        };
        <bridge> <artifact> def apply(v1: Object): Object = {
          stateMachine$async.this.apply(v1.$asInstanceOf[scala.util.Try]());
          scala.runtime.BoxedUnit.UNIT
        };
      };
   */
  def asyncTransform(asyncBody: Tree, applySym: Symbol, trParamSym: Symbol): (Tree, List[Tree]) = {
    val futureSystem = currentTransformState.futureSystem
    val futureSystemOps = futureSystem.mkOps(global)

    val asyncPos = asyncBody.pos
    val stateMachineClass = applySym.owner

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

    val liftedFields: List[Tree] = liftables(asyncBlock.asyncStates)

    // live variables analysis
    // the result map indicates in which states a given field should be nulled out
    val assignsOf = fieldsToNullOut(asyncBlock.asyncStates, liftedFields)

    for ((state, flds) <- assignsOf) {
      val assigns = flds.map { fld =>
        val fieldSym = fld.symbol
        Assign(gen.mkAttributedStableRef(thisType(fieldSym.owner), fieldSym), mkZero(fieldSym.info, asyncPos))

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
    object UseFields extends TypingTransformer(currentTransformState.unit) {
      private def fieldSel(tree: Tree) =
        atPos(tree.pos)(Select(This(stateMachineClass).setType(stateMachineClass.tpe), tree.symbol).setType(tree.symbol.tpe))
      override def transform(tree: Tree): Tree = tree match {
        case _ if currentOwner == stateMachineClass =>
          super.transform(tree)
        case ValDef(_, _, _, rhs) if liftedSyms(tree.symbol) =>
          assignUnitType(treeCopy.Assign(tree, fieldSel(tree), transform(rhs.changeOwner((tree.symbol, currentOwner)))))
        case _: DefTree if liftedSyms(tree.symbol)           => EmptyTree
        case Ident(name) if liftedSyms(tree.symbol)          => fieldSel(tree).setType(tree.tpe)
        case _                                               => super.transform(tree)
      }
    }

    val liftablesUseFields = liftedFields.map {
      case vd: ValDef => vd
      case x          => UseFields.transformAtOwner(stateMachineClass, x)
    }

    liftablesUseFields.foreach { t =>
      if (t.symbol != null && t.symbol.owner == stateMachineClass) {
        stateMachineClass.info.decls.enter(t.symbol)
        // TODO AM: refine the resetting of the lazy flag -- this is so that local lazy vals that are lifted to the class
        // actually get their LazyRef allocated to the var that holds the lazy val's reference
        t.symbol.resetFlag(Flags.LAZY)
      }
    }

    val applyBody = atPos(asyncPos)(asyncBlock.onCompleteHandler)
    val applyRhs = UseFields.transformAtOwner(applySym, applyBody)
    if (AsyncUtils.verbose) {
      val location = try asyncBody.pos.source.path catch {
        case _: UnsupportedOperationException => asyncBody.pos.toString
      }
      logDiagnostics(location, anfTree, asyncBlock, asyncBlock.asyncStates.map(_.toString))
    }
    futureSystemOps.dot(applySym, asyncBody).foreach(f => f(asyncBlock.toDot))

    (cleanupContainsAwaitAttachments(applyRhs), liftablesUseFields)
  }

  def logDiagnostics(location: String, anfTree: Tree, block: AsyncBlock, states: Seq[String]): Unit = {
    AsyncUtils.vprintln(s"In file '$location':")
    AsyncUtils.vprintln(s"ANF transform expands to:\n $anfTree")
    states foreach (s => AsyncUtils.vprintln(s))
    AsyncUtils.vprintln("===== DOT =====")
    AsyncUtils.vprintln(block.toDot)
  }
}
