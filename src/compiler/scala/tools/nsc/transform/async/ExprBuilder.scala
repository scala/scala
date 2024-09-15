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
import scala.collection.mutable.ListBuffer

trait ExprBuilder extends TransformUtils with AsyncAnalysis {
  import global._

  private def stateAssigner  = currentTransformState.stateAssigner
  private def labelDefStates = currentTransformState.labelDefStates

  private object replaceResidualJumpsWithStateTransitions extends AstTransformer {
    override def transform(tree: Tree): Tree = {
      // TODO: This is only needed for Scala.js compatibility.
      //       See https://github.com/scala/scala/pull/8816#issuecomment-640725321
      //       Perhaps make it conditional?
      // if (global.currentRun.phaseNamed("jscode") != NoPhase) tree else

      super.transform(tree) match {
        case ap @ Apply(i @ Ident(_), Nil) if isCaseLabel(i.symbol) || isMatchEndLabel(i.symbol) =>
          currentTransformState.labelDefStates.get(i.symbol) match {
            case Some(state) =>
              Block(StateTransitionStyle.UpdateAndContinue.trees(state, new StateSet), typedCurrentPos(literalUnit)).setType(definitions.UnitTpe)
            case None => ap
          }
        case transformed => transformed
      }
    }
  }
  final class AsyncState(var stats: List[Tree], val state: Int, var nextStates: Array[Int], val isEmpty: Boolean) {
    def hasNonTerminalNextState: Boolean = {
      var i = 0
      val ns = nextStates
      while (i < ns.length) {
        if (ns(i) != StateAssigner.Terminal) return true
        i += 1
      }
      false
    }
    def mkHandlerCaseForState: CaseDef = {
      replaceResidualJumpsWithStateTransitions.transform(CaseDef(Literal(Constant(state)), EmptyTree, adaptToUnit(stats))).asInstanceOf[CaseDef]
    }

    private def mkToString = s"AsyncState #$state, next = ${nextStates.toList}"
    override def toString: String = mkToString //+ " (was: " + initToString + ")"
    // private val initToString = mkToString
    def insertNullAssignments(preNulls: Iterator[Symbol], postNulls: Iterator[Symbol]): Unit = {
      val stats1 = mutable.ListBuffer[Tree]()
      def addNullAssigments(syms: Iterator[Symbol]): Unit = {
        for (fieldSym <- syms) {
          stats1 += typedCurrentPos(Assign(currentTransformState.memberRef(fieldSym), gen.mkZero(fieldSym.info)))
        }
      }
      // Add pre-state null assigments at the beginning.
      addNullAssigments(preNulls)
      var foundStateTransition = false
      stats.foreach {
        stat =>
          stats1 += stat
          if (stat.attachments.containsElement(StateTransitionTree)) {
            assert(!foundStateTransition, "cannot find more than one state transition")
            foundStateTransition = true
            // Insert post-state null assignments immediately after the state transition
            addNullAssigments(postNulls)
          }
      }
      if (!foundStateTransition) {
        addNullAssigments(postNulls)
      }
      stats = stats1.toList
    }
  }

  /*
   * Builder for a single state of an async expression.
   */
  private final class AsyncStateBuilder(val state: Int, val owner: AsyncBlockBuilder) {
    /* Statements preceding an await call. */
    val stats                      = ListBuffer[Tree]()

    val nextStates: StateSet = new StateSet
    private val jumpReplacer = new JumpReplacer(nextStates, shouldReplace(_))

    private def shouldReplace(target: Symbol): Boolean = labelDefStates.contains(target) || {
      val patternOwnerOption = owner.outerIterator.find(_.patternSyms.contains(target))
      patternOwnerOption match {
        case Some(patternOwner) =>
          // The target of this jump is owned by an enclosing block builder

          if (patternOwner != owner && patternOwner.currState == owner.currState) {
            assert(owner.currState == owner.startState, (owner.currState, owner.startState))
            // .. but we are still in our first state which is also the current state of the
            // enclosing, then the current section of code will be `incorporate`d
            // into a branch of normal control flow in the enclosing builder, at which
            // point we need can reach the jump target without a state transition.
            false
          } else {
            // ... otherwise, indicate that this case symbol needs to be addressable as a state
            // and that this label jump should be replaced by a state transition.
            true
          }
        case _ => false
      }
    }

    def +=(stat: Tree): this.type = {
      stats ++= jumpReplacer.atOwner(currentTransformState.localTyper.context.owner) {
        jumpReplacer.apply(stat)
      }
      this
    }

    private[this] var built: Boolean = false
    /** Build the state using the accumulated `stats` followed by a state transition. */
    def build(nextState: Int, style: StateTransitionStyle): AsyncState = {
      assert(!built, "cannot build after already built")
      built = true
      // Record whether this state was free of meaningful stats (excluding unit literals which creep in after
      // the ANF and state machine transforms and the state transition code added below.
      //
      // Empty stats that have a single successor state will be eliminated in `filterStates`.
      val isEmpty = !style.isInstanceOf[StateTransitionStyle.UpdateAndAwait] && stats.forall(isLiteralUnit)
      val allNextStates = new StateSet
      val concludesWithJump = stats.lastOption match {
        case Some(Apply(fun, args)) if isLabel(fun.symbol) => true
        case _ => false
      }
      def completeAsyncBlock(): Unit = {
        if (isUnitType(currentTransformState.asyncType)) {
          stats += completeSuccess(literalBoxedUnit)
        } else if (currentTransformState.asyncType.typeSymbol == definitions.NothingClass) {
          // An exception should bubble out to the enclosing handler, don't insert a complete call.
        } else {
          val expr = stats.remove(stats.size - 1)
          stats += completeSuccess(expr)
        }

        allNextStates += nextState
        stats += typedCurrentPos(Return(literalUnit).setSymbol(currentTransformState.applySym))
      }
      if (state == StateAssigner.Terminal) {
        // noop
      } else if (nextState == StateAssigner.Terminal)
        completeAsyncBlock()
      else if (!concludesWithJump)
        stats ++= style.trees(nextState, allNextStates)

      nextStates.foreach(allNextStates += _)
      new AsyncState(stats.toList, state, allNextStates.toArray, isEmpty)
    }

    override def toString: String = {
      val statsBeforeAwait = stats.mkString("\n")
      s"ASYNC STATE:\n$statsBeforeAwait"
    }
  }

  /**
   * An `AsyncBlockBuilder` builds a `ListBuffer[AsyncState]` based on the expressions of a `Block(stats, expr)` (see `Async.asyncImpl`).
   *
   * @param stats       a list of expressions
   * @param expr        the last expression of the block
   * @param startState  the start state
   * @param endState    the state to continue with
   * @param outer       the enclosing block buildemr or `None` if this is the root builder.
   */
  final private class AsyncBlockBuilder(stats: List[Tree], expr: Tree, val startState: Int, val endState: Int,
                                        startToEndUpdateStyle: StateTransitionStyle,
                                        val outer: Option[AsyncBlockBuilder] = None) {
    val patternSyms: Set[Symbol] = (expr :: stats).collect {
      case ld: LabelDef if isMatchEndLabel(ld.symbol) || isCaseLabel(ld.symbol) => ld.symbol
    }.toSet
    private var stateBuilder = new AsyncStateBuilder(startState, this)
    private val statesMap = mutable.LinkedHashMap[Int, AsyncState]()
    private var building = true

    def build: List[AsyncState] = {
      val result =
        try statesMap.values.toList
        finally building = false
      result
    }

    def outerIterator: Iterator[AsyncBlockBuilder] = Iterator.iterate(this)(_.outer.orNull).takeWhile(_ ne null)
    def currState: Int = stateBuilder.state

    addStats()

    private def addState(state: AsyncState): state.type = {
      assert(building, "must be building to add state")
      assert(!statesMap.contains(state.state), "Duplicate state: " + state)
      statesMap(state.state) = state
      state
    }
    def isRoot = outer.isEmpty

    private def addStats(): Unit = {
      stats.foreach(stat => add(stat, isExpr = false))
      add(expr, isExpr = true)
      if (!stateBuilder.stats.isEmpty || isRoot) {
        val style = if (currState == startState) startToEndUpdateStyle else StateTransitionStyle.UpdateAndContinue
        addState(stateBuilder.build(endState, style = style))
      }
      if (isRoot && currState != endState) {
        addState(new AsyncState(Nil, endState, Array(), isEmpty = true))
      }
    }

    private def add(stat: Tree, isExpr: Boolean): Unit = {
      def afterState() = if (isExpr) endState else stateAssigner.nextState()
      currentTransformState.currentPos = stat.pos
      stat match {
        case vd @ ValDef(mods, name, tpt, UnwrapBoxedUnit(Apply(fun, arg :: Nil))) if isAwait(fun) =>
          // The val await$0 = await(someFuture) pattern. The ANF tranform makes sure this is
          // always in statement position.
          //
          // Spawn a new state and transition to asynchronously it with `UpdateAndAwait` (ie an onComplete call)
          val afterAwaitState = stateAssigner.nextState()
          val transition = StateTransitionStyle.UpdateAndAwait(arg.changeOwner(vd.symbol, vd.symbol.owner))
          buildStateAndOpenNextState(afterAwaitState, style = transition)

          stateBuilder.stats += resumeTree(vd)

        case If(cond, thenp, elsep) if containsAwait(stat) =>
          // Emit a modified `If` in this state with each branch incorprating the
          // first state from the nested block builder.
          //
          // The pragram point after the if will be the start of a new state if either of the branches
          // contains an async boundary that falls through to that point.
          checkForUnsupportedAwait(cond)

          val afterIfState = afterState()
          var needAfterIfState = false
          def mkBranch(tree: Tree): Tree = {
            val (inlinedState, nestedStates) = buildNestedStatesFirstForInlining(tree,  afterIfState)
            val branchNeedsAfterIfState = incorporate(nestedStates, afterIfState)
            needAfterIfState ||= branchNeedsAfterIfState
            adaptToUnit(inlinedState.stats)
          }
          stateBuilder.stats += treeCopy.If(stat, cond, mkBranch(thenp), mkBranch(elsep))
          if (needAfterIfState) {
            stateBuilder.nextStates += afterIfState
            buildStateAndOpenNextState(afterIfState, style = StateTransitionStyle.UpdateAndContinue)
          }

        case Match(scrutinee, cases) if containsAwait(stat) =>
          // This code path is now only used for patterns which a `@switch`-able scrutinee type.
          // Translation is the same as `If`, just with more branches.

          checkForUnsupportedAwait(scrutinee)

          val afterMatchState = afterState()
          var needAfterMatchState = false
          def mkBranch(tree: Tree): Tree = {
            val (inlinedState, nestedStates) = buildNestedStatesFirstForInlining(tree,  afterMatchState)
            val branchNeedsAfterMatchState = incorporate(nestedStates, afterMatchState)
            needAfterMatchState ||= branchNeedsAfterMatchState
            adaptToUnit(inlinedState.stats)
          }

          val newCases = cases.map {
            case cd @ CaseDef(pat, guard, rhs) =>
              checkForUnsupportedAwait(guard)
              treeCopy.CaseDef(cd, pat, guard, mkBranch(rhs))
          }
          stateBuilder.stats += treeCopy.Match(stat, scrutinee, newCases)

          if (needAfterMatchState) {
            stateBuilder.nextStates += afterMatchState
            buildStateAndOpenNextState(afterMatchState, StateTransitionStyle.UpdateAndContinue)
          }

        case ld @ LabelDef(name, params, rhs) =>
          if (isCaseLabel(ld.symbol) || (isMatchEndLabel(ld.symbol) && labelDefStates.contains(ld.symbol))) {
            // LabelDefs from patterns are a bit trickier as they can (forward) branch to each other.

            labelDefStates.get(ld.symbol).foreach { startLabelState =>
              // While processing a prior `stat`, `JumpReplacer` detected that that this label was the target
              // of a jump from some code that followed an async boundary. That's common for matchEnd but
              // could also be true for a "case" label when the preceding pattern had an
              // async guard or extractor.
              //
              // We rely on the fact that the patterm matcher only emits forward branches.
              // This allows analysis and transformation to occur in one pass.
              stateBuilder.nextStates += startLabelState
              buildStateAndOpenNextState(startLabelState, StateTransitionStyle.Update)
            }

            val afterLabelState = afterState()
            val (inlinedState, nestedStates) = buildNestedStatesFirstForInlining(rhs, afterLabelState)

            // Leave this label here for synchronous jumps from previous cases. This is
            // allowed even if this case has its own state (ie if there is an asynchrounous path
            // from the start of the pattern to this case/matchEnd)
            ld.symbol.setInfo(MethodType(Nil, definitions.UnitTpe))
            stateBuilder.stats += treeCopy.LabelDef(ld, ld.name, ld.params, adaptToUnit(inlinedState.stats))

            val needsAfterLabelState = incorporate(nestedStates, afterLabelState)
            if (needsAfterLabelState) {
              buildStateAndOpenNextState(afterLabelState, style = StateTransitionStyle.None)
            }
          } else if (containsAwait(rhs)) {
            // A while loop containing an await. We assuming that the backward branch is reachable across the async
            // code path and create a state for the `while` label.
            //
            // In theory we could avoid creating this state in code like:
            //
            //   while (cond) { if (z) { await(f); return }; i += 1 }
            //
            val startLabelState = addLabelState(ld.symbol)
            val afterLabelState = afterState()
            val nestedStates = buildNestedStates(rhs, startLabelState, afterLabelState)
            nestedStates.foreach(addState)
            buildStateAndOpenNextState(startLabelState, afterLabelState, StateTransitionStyle.UpdateAndContinue)
          } else {
            checkForUnsupportedAwait(stat)
            stateBuilder += stat
          }
        case _ =>
          checkForUnsupportedAwait(stat)
          stateBuilder += stat
      }
    }

    private def buildNestedStatesFirstForInlining(nestedTree: Tree, endState: Int): (AsyncState, List[AsyncState]) = {
      val (nestedStats, nestedExpr) = statsAndExpr(nestedTree)
      val nestedBuilder = new AsyncBlockBuilder(nestedStats, nestedExpr, currState, endState, StateTransitionStyle.None, Some(this))
      val (inlinedState :: Nil, nestedStates) = nestedBuilder.build.partition(_.state == currState): @unchecked
      inlinedState.nextStates.foreach(stateBuilder.nextStates += _)
      (inlinedState, nestedStates)
    }
    private def buildNestedStates(nestedTree: Tree, startState: Int, endState: Int): List[AsyncState] = {
      val (nestedStats, nestedExpr) = statsAndExpr(nestedTree)
      val nestedBuilder = new AsyncBlockBuilder(nestedStats, nestedExpr, startState, endState, StateTransitionStyle.Update, Some(this))
      nestedBuilder.build
    }

    private def buildStateAndOpenNextState(nextState: Int, style: StateTransitionStyle): Unit =
      buildStateAndOpenNextState(nextState, nextState, style)
    private def buildStateAndOpenNextState(toState: Int, nextState: Int, style: StateTransitionStyle): Unit = {
      addState(stateBuilder.build(toState, style))
      stateBuilder = new AsyncStateBuilder(nextState, this)
    }

    private def checkForUnsupportedAwait(tree: Tree) = if (containsAwait(tree)) {
      reportUnsupportedAwaits(tree)
    }

    /** Copy these states into the current block builder's async stats updating the open state builder's
     *  next states
     *
     *  @return true if any of the nested states includes `afterState` as a next state.
     */
    private def incorporate(nestedStates: List[AsyncState], afterState: Int): Boolean = {
      def loop(states: List[AsyncState], needsAfterState: Boolean): Boolean = states match {
        case Nil => needsAfterState
        case state :: rest =>
          addState(state)
          loop(rest, needsAfterState || state.nextStates.contains(afterState))
      }
      loop(nestedStates, needsAfterState = false)
    }
  }

  trait AsyncBlock {
    def asyncStates: List[AsyncState]

    def onCompleteHandler: Tree

    def toDot: String
  }

  /**
   * Uses `AsyncBlockBuilder` to create an instance of `AsyncBlock`.
   *
   * @param  block      a `Block` tree in ANF
   * @return            an `AsyncBlock`
   */
  def buildAsyncBlock(block: Block): AsyncBlock = {
    val Block(stats, expr) = block
    val startState = stateAssigner.nextState()
    val endState = StateAssigner.Terminal

    val blockBuilder = new AsyncBlockBuilder(stats, expr, startState, endState, startToEndUpdateStyle = StateTransitionStyle.Update)

    new AsyncBlock {
      private val switchIds = mutable.HashMap[Integer, Integer]()
      private val emptyReplacements = mutable.HashMap[Integer, Integer]()
      private def switchIdOf(state: Integer) = switchIds(emptyReplacements.getOrElse(state, state))

      // render with http://graphviz.it/#/new
      def toDot: String = {
        val states = asyncStates
        def toHtmlLabel(label: String, preText: String, builder: StringBuilder): Unit = {
          val br = "<br align=\"left\"/>"
          builder.append("<b>").append(label).append("</b>").append("<br/>")
          builder.append("<font face=\"Courier\">")
          preText.split("\n").foreach {
            (line: String) =>
              builder.append(br)
              // TODO Wrap with CDATA instead?
              builder.append(
                line.replace("&", "&amp;")
                    .replace("\"", "&quot;")
                    .replace("<", "&lt;")
                    .replace(">", "&gt;")
                    .replace(" ", "&nbsp;")
              )
          }
          builder.append(br)
          builder.append("</font>")
        }
        val dotBuilder = new StringBuilder()
        dotBuilder.append("digraph {\n")
        def stateLabel(s: Int) = {
          if (s == 0) "INITIAL" else if (s == StateAssigner.Terminal) "TERMINAL" else (if (compactStates) switchIdOf(s) else s).toString
        }
        for (state <- asyncStates) {
          dotBuilder.append(s"""${stateLabel(state.state)} [label=""").append("<")
          def show(t: Tree): String = {
            (t match {
              case Block(stats, expr) => stats ::: expr :: Nil
              case t => t :: Nil
            }).iterator.map(t => global.show(t)).mkString("\n")
          }
          val CaseDef(_, _, body) = state.mkHandlerCaseForState
          toHtmlLabel(stateLabel(state.state), show(compactStateTransform.transform(body)), dotBuilder)
          dotBuilder.append("> ]\n")
        }
        for (state <- states) {
          for (succ <- state.nextStates) {
            val style = ""
            dotBuilder.append(s"""${stateLabel(state.state)} -> ${stateLabel(succ)} $style""")
            dotBuilder.append("\n")
          }
        }
        dotBuilder.append("}\n")
        dotBuilder.toString
      }

      lazy val asyncStates: List[AsyncState] = filterStates(blockBuilder.build) // drop the terminal state which contains no code

      /**
       * Builds the definition of the `apply(tr: Try)` method.
       */
      def onCompleteHandler: Tree = {
        val transformState = currentTransformState
        def stateMemberRef = gen.mkApplyIfNeeded(transformState.memberRef(transformState.stateGetter))

        val asyncStatesInit = asyncStates.init // drop the terminal state which has no code.
        val throww          = Throw(Apply(Select(New(Ident(IllegalStateExceptionClass)), IllegalStateExceptionClass_NEW_String), List(gen.mkMethodCall(currentRun.runDefinitions.String_valueOf_Int, stateMemberRef :: Nil))))
        val body            =
          typedBasePos(Match(stateMemberRef,
                      asyncStatesInit.map(_.mkHandlerCaseForState) ++
                        List(CaseDef(Ident(nme.WILDCARD), EmptyTree,
                                     throww))))
        val body1      = compactStates(body.asInstanceOf[Match])
        val stateMatch = if (transformState.allowExceptionsToPropagate) {
          body1
        } else {
          Try(
            body1,
            List(
              CaseDef(
                Bind(nme.t, Typed(Ident(nme.WILDCARD), Ident(definitions.ThrowableClass))),
                EmptyTree,
                Block(Apply(currentTransformState.memberRef(currentTransformState.stateCompleteFailure), Ident(nme.t) :: Nil) :: Nil, Return(literalUnit))
                )
              ), EmptyTree)
        }
        typedBasePos(LabelDef(transformState.whileLabel, Nil, Block(stateMatch :: Nil, Apply(Ident(transformState.whileLabel), Nil))))
      }

      private def compactStates = true

      // Filter out dead or trivial states.
      private def filterStates(all: List[AsyncState]): List[AsyncState] = if (compactStates) {
        val (initial :: Nil, rest) = all.partition(_.state == blockBuilder.startState): @unchecked
        val map = all.iterator.map(x => (x.state, x)).toMap
        val seen = mutable.HashSet[Int]()
        seen.addOne(all.last.state)
        def followEmptyState(state: AsyncState): AsyncState = if (state.isEmpty && state.nextStates.size == 1) {
          val next = state.nextStates(0)
          if (next == blockBuilder.endState) state
          else followEmptyState(map(next))
        } else state
        all.foreach {state =>
          val state1 = followEmptyState(state)
          if ((state1 ne state) && state.state != StateAssigner.Initial)
            emptyReplacements(state.state) = state1.state
        }
        all.foreach {
          state => state.nextStates = state.nextStates.map(s => emptyReplacements.getOrElse[Integer](s, s).toInt).distinct
        }
        def loop(state: AsyncState): Unit = {
          if (!emptyReplacements.contains(state.state))
            seen.addOne(state.state)
          for (i <- state.nextStates if !seen.contains(i) && i != StateAssigner.Terminal) {
            loop(map(i))
          }
        }
        loop(initial)
        val live = initial :: rest.filter(state => seen(state.state))
        var nextSwitchId = 0
        live.foreach { state =>
          val switchId = nextSwitchId
          switchIds(state.state) = switchId
          nextSwitchId += 1
        }
        live
      } else all

      private val compactStateTransform = new AstTransformer {
        val transformState = currentTransformState
        override def transform(tree: Tree): Tree = tree match {
          case Apply(qual: Select, (lit @ Literal(Constant(i: Integer))) :: Nil) if qual.symbol == transformState.stateSetter && compactStates =>
            val replacement = switchIdOf(i)
            treeCopy.Apply(tree, qual, treeCopy.Literal(lit, Constant(replacement)):: Nil)
          case _: Match | _: CaseDef | _: Block | _: If | _: LabelDef =>
            super.transform(tree)
          case _ => tree
        }
      }

      private def compactStates(m: Match): Tree = if (!compactStates) m else {
        val casesAndReplacementIds: List[(Integer, CaseDef)] = m.cases.map {
          case cd @ CaseDef(lit @ Literal(Constant(i: Integer)), EmptyTree, rhs) =>
            val replacement = switchIdOf(i)
            val rhs1 = compactStateTransform.transform(rhs)
            (replacement, treeCopy.CaseDef(cd, treeCopy.Literal(lit, Constant(replacement)), EmptyTree, rhs1))
          case cd =>
            (Int.box(Int.MaxValue), cd) // sort the default case to the end.
        }
        val cases1: List[CaseDef] = casesAndReplacementIds.sortBy(_._1).map(_._2)
        treeCopy.Match(m, m.selector, cases1)
      }
    }
  }
  // Resume execution by extracting the successful value and assigining it to the `awaitable.resultValDef`
  private def resumeTree(awaitableResult: ValDef): Tree = {
    def tryyReference = gen.mkAttributedIdent(currentTransformState.applyTrParam)
    deriveValDef(awaitableResult) { _ =>
      if (currentTransformState.tryGetIsIdentity) {
        tryyReference
      } else {
        val temp = awaitableResult.symbol.newTermSymbol(nme.trGetResult).setInfo(definitions.ObjectTpe)
        val tempVd = ValDef(temp, gen.mkMethodCall(currentTransformState.memberRef(currentTransformState.stateTryGet), tryyReference :: Nil))
        typedCurrentPos(Block(
          tempVd :: Nil,
        If(Apply(gen.mkAttributedSelect(currentTransformState.stateMachineRef(), definitions.Object_eq), gen.mkAttributedIdent(temp) :: Nil),
             Return(literalUnit),
             gen.mkCast(gen.mkAttributedIdent(temp), tempVd.symbol.info)
             )
          ))
      }
    }
  }

  // Comlete the Promise in the `result` field with the final successful result of this async block.
  private def completeSuccess(expr: Tree): Tree = {
    deriveTree(expr, definitions.UnitTpe) { expr =>
      typedCurrentPos(Apply(currentTransformState.memberRef(currentTransformState.stateCompleteSuccess), expr :: Nil))
    }
  }

  /** What trailing statements should be added to the code for this state to transition to the nest state? */
  private sealed abstract class StateTransitionStyle {
    def trees(next: Int, stateSet: StateSet): List[Tree]
    protected def mkStateTree(nextState: Int): Tree = {
      val transformState = currentTransformState
      val callSetter = Apply(transformState.memberRef(transformState.stateSetter), Literal(Constant(nextState)) :: Nil)
      typedCurrentPos(callSetter.updateAttachment(StateTransitionTree))
    }
  }

  private object StateTransitionStyle {
    /** Do not update the state variable  */
    case object None extends StateTransitionStyle {
      def trees(nextState: Int, stateSet: StateSet): List[Tree] = Nil
    }
    /** Update the state variable, */
    case object Update extends StateTransitionStyle {
      def trees(nextState: Int, stateSet: StateSet): List[Tree] = {
        stateSet += nextState
        List(mkStateTree(nextState))
      }
    }
    /** Update the state variable and the await completion of `awaitble.expr`. */
    case class UpdateAndAwait(awaitable: Tree) extends StateTransitionStyle {
      def trees(nextState: Int, stateSet: StateSet): List[Tree] = {
        stateSet += nextState
        val transformState = currentTransformState

        // Suspend execution by calling `onComplete` with the state machine itself as a callback.
        //
        // If the state machine contains a member `getCompleted`, this first be called to see if
        // an already-completed result is avaialble. If so, execution will continue on this thread
        // (_without_ consuming an extra stack frome!)

        def callOnComplete(fut: Tree): Tree =
          Apply(currentTransformState.memberRef(transformState.stateOnComplete), fut :: Nil)

        val runCompletedOnSameThread = transformState.stateGetCompleted != NoSymbol
        if (runCompletedOnSameThread) {
          val tempAwaitableSym = transformState.applyTrParam.owner.newTermSymbol(nme.awaitable).setInfo(awaitable.tpe)
          val initAwaitableTemp = ValDef(tempAwaitableSym, awaitable)
          val initTempCompleted = Assign(
            gen.mkAttributedIdent(transformState.applyTrParam),
            gen.mkMethodCall(transformState.memberRef(transformState.stateGetCompleted), gen.mkAttributedIdent(tempAwaitableSym) :: Nil)
          )
          val null_ne = gen.mkAttributedSelect(Literal(Constant(null)).setType(definitions.NullTpe), definitions.Any_!=)
          val ifTree =
            If(Apply(null_ne, Ident(transformState.applyTrParam) :: Nil),
              Apply(Ident(transformState.whileLabel), Nil),
              Block(toStats(callOnComplete(gen.mkAttributedIdent(tempAwaitableSym))), Return(literalUnit).setSymbol(transformState.applySym)))
          typedCurrentPos(initAwaitableTemp) :: typedCurrentPos(initTempCompleted) :: mkStateTree(nextState) :: typedCurrentPos(ifTree) :: Nil
        } else {
          mkStateTree(nextState) :: toStats(typedCurrentPos(callOnComplete(awaitable))) ::: typedCurrentPos(Return(literalUnit)) :: Nil
        }
      }
    }

    /** Update the state variable and jump to the while loop that encloses the state machine. */
    case object UpdateAndContinue extends StateTransitionStyle {
      def trees(nextState: Int, stateSet: StateSet): List[Tree] = {
        stateSet += nextState
        List(mkStateTree(nextState), typedCurrentPos(Apply(Ident(currentTransformState.whileLabel), Nil)))
      }
    }
  }

  private def toStats(tree: Tree): List[Tree] = tree match {
    case Block(stats, expr) if isLiteralUnit(expr) => stats
    case Block(stats, expr) => stats ::: (expr :: Nil)
    case _ => tree :: Nil
  }

  def addLabelState(label: Symbol): Int =
    labelDefStates.getOrElseUpdate(label, StateAssigner.stateIdForLabel(label))

  // Replace jumps to qualifying labels as a state transition.
  private class JumpReplacer(states: StateSet, shouldReplace: global.Symbol => Boolean)
    extends ThicketTransformer(currentTransformState.localTyper) {
    val initOwner = currentTransformState.localTyper.context.owner
    override def transform(tree: Tree): Tree = tree match {
      case _ if initOwner != currentOwner.enclMethod => tree
      case Apply(fun, args) if isLabel(fun.symbol) =>
        if (shouldReplace(fun.symbol)) {
          val nextState = addLabelState(fun.symbol)
          val trees = StateTransitionStyle.UpdateAndContinue.trees(nextState, states)
          localTyper.typed(Thicket(listToBlock(trees)))
        } else {
          super.transform(tree)
        }
      case _ =>
        super.transform(tree)
    }
  }
}
