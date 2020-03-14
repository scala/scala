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
import scala.tools.nsc.transform.TypingTransformers

// TODO: check there's no await outside of an async block
trait AsyncTransform extends AnfTransform with AsyncAnalysis with Lifter with LiveVariables with TypingTransformers {
  var currentTransformState: AsyncTransformState[global.type] = _
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
  def asyncTransform(asyncBody: Tree): (Tree, List[Tree]) = {
    val transformState = currentTransformState
    import transformState.{applySym, stateMachineClass}
    val futureSystem = currentTransformState.futureSystem
    val futureSystemOps = futureSystem.mkOps(global)

    val asyncPos = asyncBody.pos

    markContainsAwait(asyncBody) // ANF transform also relies on whether something contains await
    reportUnsupportedAwaits(asyncBody)

    // Transform to A-normal form:
    //  - no await calls in qualifiers or arguments,
    //  - if/match only used in statement position.
    val anfTree0: Block = anfTransform(asyncBody, applySym)

    val anfTree = futureSystemOps.postAnfTransform(anfTree0)

    // The ANF transform re-parents some trees, so the previous traversal to mark ancestors of
    // await is no longer reliable.
    cleanupContainsAwaitAttachments(anfTree)
    markContainsAwait(anfTree)

    val asyncBlock = buildAsyncBlock(anfTree)

    val liftedFields: List[Tree] = liftables(asyncBlock.asyncStates)

    // live variables analysis
    // the result map indicates in which states a given field should be nulled out
    val nullOut = true
    if (nullOut) {
      val assignsOf = fieldsToNullOut(asyncBlock.asyncStates, asyncBlock.asyncStates.last, liftedFields)

      for ((state, flds) <- assignsOf) {
        val asyncState = asyncBlock.asyncStates.find(_.state == state).get
        val stats1 = mutable.ListBuffer[Tree]()
        def addNullAssigments(): Unit = {
          // Insert the null assignments immediately after the state transition
          for (fld <- flds) {
            val fieldSym = fld.symbol
            stats1 += typed(Assign(gen.mkAttributedStableRef(fieldSym.owner.thisPrefix, fieldSym), gen.mkZero(fieldSym.info).setPos(asyncPos)))
          }
        }
        var foundStateTransition = false
        asyncState.stats.foreach {
          stat =>
            stats1 += stat
            if (stat.attachments.containsElement(StateTransitionTree)) {
              assert(!foundStateTransition)
              foundStateTransition = true
              // Insert the null assignments immediately after the state transition
              addNullAssigments()
            }
        }
        if (!foundStateTransition) {
          addNullAssigments()
        }
        asyncState.stats = stats1.toList
      }
    }

    val liftedSyms = liftedFields.map(_.symbol).toSet
    liftedSyms.foreach { sym =>
      if (sym != null) {
        sym.owner = stateMachineClass
        if (sym.isModule)
          sym.asModule.moduleClass.owner = stateMachineClass
      }
    }

    // Adjust the tree to:
    //   - lifted local variables are entered into the scope of the state machine class
    //   - references to them are rewritten as referencs to the fields.
    //   - the rhs of ValDefs that initialize such fields is turned into an assignment to the field
    object UseFields extends explicitOuter.OuterPathTransformer(currentTransformState.localTyper) {
      private def fieldSel(tree: Tree) = {
        assert(currentOwner != NoSymbol)
        val outerOrThis = if (stateMachineClass == currentClass) gen.mkAttributedThis(stateMachineClass) else {
          // These references need to be selected from an outer reference, because explicitouter
          // has already run we must perform this transform explicitly here.
          tree.symbol.makeNotPrivate(tree.symbol.owner)
          outerPath(outerValue, currentClass.outerClass, stateMachineClass)
        }
        atPos(tree.pos)(Select(outerOrThis.setType(stateMachineClass.tpe), tree.symbol).setType(tree.symbol.tpe))
      }
      override def transform(tree: Tree): Tree = tree match {
        case ValDef(_, _, _, rhs) if liftedSyms(tree.symbol) && currentOwner == applySym =>
          // Drop the lifted definitions from the apply method
          assignUnitType(treeCopy.Assign(tree, fieldSel(tree), transform(rhs.changeOwner(tree.symbol, currentOwner))))
        case _: DefTree if liftedSyms(tree.symbol) && currentOwner == applySym =>
          // Drop the lifted definitions from the apply method
          EmptyTree
        case md: MemberDef if currentOwner == stateMachineClass && liftedSyms(tree.symbol)=>
          stateMachineClass.info.decls.enter(md.symbol)
          super.transform(tree)
        case Assign(i @ Ident(name), rhs) if liftedSyms(i.symbol) =>
          treeCopy.Assign(tree, fieldSel(i), adapt(transform(rhs), i.symbol.tpe))
        case Ident(name) if liftedSyms(tree.symbol) =>
          fieldSel(tree).setType(tree.tpe)
        case _ =>
          super.transform(tree)
      }

      // Use localTyper to adapt () to BoxedUnit in `val ifRes: Object; if (cond) "" else ()`
      private def adapt(tree: Tree, pt: Type): Tree = localTyper.typed(tree, pt)
    }
    val applyBody = atPos(asyncPos)(asyncBlock.onCompleteHandler)
    val ClassDef(_, _, _, Template(_, _, (applyDefDef: DefDef) :: liftablesUseFields)) =
      UseFields.transformAtOwner(stateMachineClass.owner, ClassDef(stateMachineClass, DefDef(applySym, applyBody) :: liftedFields))

    if (settings.debug.value && shouldLogAtThisPhase) {
      val location = try asyncBody.pos.source.path catch {
        case _: UnsupportedOperationException => asyncBody.pos.toString
      }
      logDiagnostics(location, anfTree, asyncBlock, asyncBlock.asyncStates.map(_.toString))
    }
    futureSystemOps.dot(applySym, asyncBody).foreach(f => f(asyncBlock.toDot))

    (cleanupContainsAwaitAttachments(applyDefDef.rhs), liftablesUseFields)
  }

  def logDiagnostics(location: String, anfTree: Tree, block: AsyncBlock, states: Seq[String]): Unit = {
    inform(s"In file '$location':")
    inform(s"ANF transform expands to:\n $anfTree")
    states foreach (s => inform(s))
    inform("===== DOT =====")
    inform(block.toDot)
  }
}
