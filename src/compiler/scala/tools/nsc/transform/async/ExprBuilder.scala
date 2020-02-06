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

import java.util.function.IntUnaryOperator

import user.FutureSystem
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.existentials

trait ExprBuilder extends TransformUtils {
  import global._

  def tryAny = transformType(currentTransformState.ops.tryType(definitions.AnyTpe))

  private def stateAssigner  = currentTransformState.stateAssigner
  private def labelDefStates = currentTransformState.labelDefStates

  trait AsyncState {
    def state: Int

    def nextStates: Array[Int]

    def mkHandlerCaseForState[T]: CaseDef

    def mkOnCompleteHandler[T]: Option[CaseDef] = None

    var stats: List[Tree]

    def treeThenStats(tree: Tree): List[Tree] =
      adaptToUnitIgnoringNothing(tree :: stats) :: Nil

    final def allStats: List[Tree] = this match {
      case a: AsyncStateWithAwait => treeThenStats(a.awaitable.resultValDef)
      case _ => stats
    }

    final def body: Tree = stats match {
      case stat :: Nil => stat
      case init :+ last => Block(init, last)
    }
  }

  /** A sequence of statements that concludes with a unconditional transition to `nextState` */
  final class SimpleAsyncState(var stats: List[Tree], val state: Int, nextState: Int, symLookup: SymLookup)
    extends AsyncState {

    val nextStates: Array[Int] =
      Array(nextState)

    def mkHandlerCaseForState[T]: CaseDef = {
      mkHandlerCase(state, treeThenStats(mkStateTree(nextState, symLookup)))
    }

    override val toString: String =
      s"AsyncState #$state, next = $nextState"
  }

  /** A sequence of statements with a conditional transition to the next state, which will represent
    * a branch of an `if` or a `match`.
    */
  final class AsyncStateWithoutAwait(var stats: List[Tree], val state: Int, val nextStates: Array[Int]) extends AsyncState {
    override def mkHandlerCaseForState[T]: CaseDef =
      mkHandlerCase(state, stats)

    override val toString: String =
      s"AsyncStateWithoutAwait #$state, nextStates = ${nextStates.toList}"
  }

  /** A sequence of statements that concludes with an `await` call. The `onComplete`
    * handler will unconditionally transition to `nextState`.
    */
  final class AsyncStateWithAwait(var stats: List[Tree], val state: Int, val onCompleteState: Int, nextState: Int,
                                  val awaitable: Awaitable, symLookup: SymLookup)
    extends AsyncState {

    val nextStates: Array[Int] =
      Array(nextState)

    override def mkHandlerCaseForState[T]: CaseDef = {
      val futureSystem = currentTransformState.futureSystem
      val futureSystemOps = futureSystem.mkOps(global)
      val fun = This(tpnme.EMPTY)
      val callOnComplete = futureSystemOps.onComplete[Any, Unit](awaitable.expr,
        fun, Ident(nme.execContext))
      val tryGetOrCallOnComplete: List[Tree] =
        if (futureSystemOps.continueCompletedFutureOnSameThread) {
          val tempName = nme.completed
          val initTemp = ValDef(NoMods, tempName, TypeTree(tryAny), futureSystemOps.getCompleted[Any](awaitable.expr))
          val null_ne = Select(Literal(Constant(null)), TermName("ne"))
          val ifTree =
            If(Apply(null_ne, Ident(tempName) :: Nil),
              adaptToUnit(ifIsFailureTree[T](Ident(tempName)) :: Nil),
              Block(toList(callOnComplete), Return(literalUnit)))

          initTemp :: ifTree :: Nil
        } else
          toList(callOnComplete) ::: Return(literalUnit) :: Nil
      mkHandlerCase(state, stats ++ List(mkStateTree(onCompleteState, symLookup)) ++ tryGetOrCallOnComplete)
    }

    /* if (tr.isFailure)
     *   result.complete(tr.asInstanceOf[Try[T]])
     * else {
     *   <resultName> = tr.get.asInstanceOf[<resultType>]
     *   <nextState>
     *   <mkResumeApply>
     * }
     */
    def ifIsFailureTree[T](tryReference: => Tree) = {
      val futureSystem = currentTransformState.futureSystem
      val futureSystemOps = futureSystem.mkOps(global)

      assert(isPastErasure)
      val tryyGet = futureSystemOps.tryyGet[Any](tryReference)

      val getAndUpdateState = Block(List(Assign(Ident(awaitable.resultName), tryyGet)), mkStateTree(nextState, symLookup))
      if (futureSystem.emitTryCatch) {
        If(futureSystemOps.tryyIsFailure(tryReference),
          Block(toList(futureSystemOps.completeProm[T](
            symLookup.selectResult,
            tryReference)),
            Return(literalUnit)),
          getAndUpdateState
        )
      } else {
        getAndUpdateState
      }
    }

    override def mkOnCompleteHandler[T]: Option[CaseDef] = {
      Some(mkHandlerCase(onCompleteState, List(ifIsFailureTree[T](Ident(symLookup.applyTrParam)))))
    }

    override val toString: String =
      s"AsyncStateWithAwait #$state, next = $nextState"
  }

  /*
   * Builder for a single state of an async expression.
   */
  final class AsyncStateBuilder(state: Int, private val symLookup: SymLookup) {
    /* Statements preceding an await call. */
    private val stats                      = ListBuffer[Tree]()
    /** The state of the target of a LabelDef application (while loop jump) */
    private var nextJumpState: Option[Int] = None
    private var nextJumpSymbol: Symbol = NoSymbol
    def effectiveNextState(nextState: Int) =
      nextJumpState.orElse(if (nextJumpSymbol == NoSymbol) None
                           else Some(stateIdForLabel(nextJumpSymbol))).getOrElse(nextState)

    def +=(stat: Tree): this.type = {
      // Allow `()` (occurs in do/while)
      assert(isLiteralUnit(stat) || nextJumpState.isEmpty, s"statement appeared after a label jump: $stat")

      def addStat() = stats += stat
      stat match {
        case Apply(fun, args) if isLabel(fun.symbol) =>
          // labelDefStates belongs to the current ExprBuilder
          labelDefStates get fun.symbol match {
            case opt@Some(nextState) =>
              // A backward jump
              nextJumpState = opt // re-use object
              nextJumpSymbol = fun.symbol
            case None =>
              // We haven't the corresponding LabelDef, this is a forward jump
              nextJumpSymbol = fun.symbol
          }
        case _               => addStat()
      }
      this
    }

    def resultWithAwait(awaitable: Awaitable,
                        onCompleteState: Int,
                        nextState: Int): AsyncState = {
      new AsyncStateWithAwait(stats.toList, state, onCompleteState, effectiveNextState(nextState), awaitable, symLookup)
    }

    def resultSimple(nextState: Int): AsyncState = {
      new SimpleAsyncState(stats.toList, state, effectiveNextState(nextState), symLookup)
    }

    def resultWithIf(condTree: Tree, thenState: Int, elseState: Int): AsyncState = {
      def mkBranch(state: Int) = mkStateTree(state, symLookup)
      this += If(condTree, mkBranch(thenState), mkBranch(elseState))
      new AsyncStateWithoutAwait(stats.toList, state, Array(thenState, elseState))
    }

    /**
     * Build `AsyncState` ending with a match expression.
     *
     * The cases of the match simply resume at the state of their corresponding right-hand side.
     *
     * @param scrutTree       tree of the scrutinee
     * @param cases           list of case definitions
     * @param caseStates      starting state of the right-hand side of the each case
     * @return                an `AsyncState` representing the match expression
     */
    def resultWithMatch(scrutTree: Tree, cases: List[CaseDef], caseStates: Array[Int], symLookup: SymLookup): AsyncState = {
      // 1. build list of changed cases
      val newCases = for ((cas, num) <- cases.zipWithIndex) yield cas match {
        case CaseDef(pat, guard, rhs) =>
          val bindAssigns = rhs.children.takeWhile(isSyntheticBindVal)
          CaseDef(pat, guard, Block(bindAssigns, mkStateTree(caseStates(num), symLookup)))
      }
      // 2. insert changed match tree at the end of the current state
      this += Match(scrutTree, newCases)
      new AsyncStateWithoutAwait(stats.toList, state, caseStates)
    }

    def resultWithLabel(startLabelState: Int, symLookup: SymLookup): AsyncState = {
      this += mkStateTree(startLabelState, symLookup)
      new AsyncStateWithoutAwait(stats.toList, state, Array(startLabelState))
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
   */
  final private class AsyncBlockBuilder(stats: List[Tree], expr: Tree, startState: Int, endState: Int,
                                        private val symLookup: SymLookup) {
    val asyncStates = ListBuffer[AsyncState]()

    var stateBuilder = new AsyncStateBuilder(startState, symLookup)
    var currState    = startState

    def checkForUnsupportedAwait(tree: Tree) = if (containsAwait(tree))
      abort(tree.pos, "await must not be used in this position")

    def nestedBlockBuilder(nestedTree: Tree, startState: Int, endState: Int) = {
      val (nestedStats, nestedExpr) = statsAndExpr(nestedTree)
      new AsyncBlockBuilder(nestedStats, nestedExpr, startState, endState, symLookup)
    }


    def nextState() = stateAssigner.nextState()
    def directlyAdjacentLabelDefs(t: Tree): List[Tree] = {
      def isPatternCaseLabelDef(t: Tree) = t match {
        case LabelDef(name, _, _) => name.toString.startsWith("case")
        case _ => false
      }
      val span = (stats :+ expr).filterNot(isLiteralUnit).span(_ ne t)
      span match {
        case (before, _ :: after) =>
          before.reverse.takeWhile(isPatternCaseLabelDef) ::: after.takeWhile(isPatternCaseLabelDef)
        case _ =>
          stats :+ expr
      }
    }

    // `while(await(x))` ... or `do { await(x); ... } while(...)` contain an `If` that loops;
    // we must break that `If` into states so that it convert the label jump into a state machine
    // transition
    private def containsForeignLabelJump(t: Tree): Boolean = {
      val labelDefs = t.collect { case ld: LabelDef => ld.symbol }.toSet
      t.exists {
        case rt: RefTree => rt.symbol != null && isLabel(rt.symbol) && !(labelDefs contains rt.symbol)
        case _ => false
      }
    }

    // unwrap Block(t :: Nil, scala.runtime.BoxedUnit.UNIT) -- erasure will add the expr when await had type Unit
    object UnwrapBoxedUnit {
      def unapply(tree: Tree): Some[Tree] = tree match {
        case Block(t :: Nil, unit) if isLiteralUnit(unit) => Some(t) // is really only going to be BoxedUnit, but hey
        case t => Some(t)
      }
    }
    // populate asyncStates
    def add(stat: Tree, afterState: Option[Int] = None): Unit = stat match {
      // the val name = await(..) pattern
      case vd @ ValDef(mods, name, tpt, UnwrapBoxedUnit(Apply(fun, arg :: Nil))) if isAwait(fun) =>
        val onCompleteState = nextState()
        val afterAwaitState = afterState.getOrElse(nextState())
        val awaitable = Awaitable(arg, stat.symbol, tpt.tpe, vd)
        asyncStates += stateBuilder.resultWithAwait(awaitable, onCompleteState, afterAwaitState) // complete with await
        currState = afterAwaitState
        stateBuilder = new AsyncStateBuilder(currState, symLookup)

      case If(cond, thenp, elsep) if containsAwait(stat) || containsForeignLabelJump(stat) =>
        checkForUnsupportedAwait(cond)

        val thenStartState = nextState()
        val elseStartState = nextState()
        val afterIfState = afterState.getOrElse(nextState())

        // the two Int arguments are the start state of the then branch and the else branch, respectively
        asyncStates += stateBuilder.resultWithIf(cond, thenStartState, elseStartState)

        List((thenp, thenStartState), (elsep, elseStartState)) foreach {
          case (branchTree, state) =>
            val builder = nestedBlockBuilder(branchTree, state, afterIfState)
            asyncStates ++= builder.asyncStates
        }

        currState = afterIfState
        stateBuilder = new AsyncStateBuilder(currState, symLookup)

      case Match(scrutinee, cases) if containsAwait(stat) =>
        checkForUnsupportedAwait(scrutinee)

        val caseStates = new Array[Int](cases.length)
        java.util.Arrays.setAll(caseStates, new IntUnaryOperator {
          override def applyAsInt(operand: Int): Int = nextState()
        })
        val afterMatchState = afterState.getOrElse(nextState())

        asyncStates += stateBuilder.resultWithMatch(scrutinee, cases, caseStates, symLookup)

        for ((cas, num) <- cases.zipWithIndex) {
          val (stats, expr) = statsAndExpr(cas.body)
          val stats1 = stats.dropWhile(isSyntheticBindVal)
          val builder = nestedBlockBuilder(Block(stats1, expr), caseStates(num), afterMatchState)
          asyncStates ++= builder.asyncStates
        }

        currState = afterMatchState
        stateBuilder = new AsyncStateBuilder(currState, symLookup)
      case ld @ LabelDef(name, params, rhs)
        if containsAwait(rhs) || directlyAdjacentLabelDefs(ld).exists(containsAwait) =>

        val startLabelState = stateIdForLabel(ld.symbol)
        val afterLabelState = afterState.getOrElse(nextState())
        asyncStates += stateBuilder.resultWithLabel(startLabelState, symLookup)
        labelDefStates(ld.symbol) = startLabelState
        val builder = nestedBlockBuilder(rhs, startLabelState, afterLabelState)
        asyncStates ++= builder.asyncStates
        currState = afterLabelState
        stateBuilder = new AsyncStateBuilder(currState, symLookup)
      case b @ Block(stats, expr) =>
        for (stat <- stats) add(stat)
        add(expr, afterState = Some(endState))
      case _ =>
        checkForUnsupportedAwait(stat)
        stateBuilder += stat
    }
    for (stat <- (stats :+ expr)) add(stat)
    val lastState = stateBuilder.resultSimple(endState)
    asyncStates += lastState
  }

  trait AsyncBlock {
    def asyncStates: List[AsyncState]

    def onCompleteHandler[T]: Tree

    def toDot: String
  }


  case class SymLookup(stateMachineClass: Symbol, applyTrParam: Symbol) {
    def stateMachineMember(name: TermName): Symbol = {
      stateMachineClass.info.member(name)
    }
    def memberRef(name: TermName): Tree =
      gen.mkAttributedRef(stateMachineClass.typeConstructor, stateMachineMember(name))
    def memberRef(sym: Symbol): Tree =
      gen.mkAttributedRef(stateMachineClass.typeConstructor, sym)

    lazy val stateGetter: Symbol = stateMachineMember(nme.state)
    lazy val stateSetter: Symbol = stateGetter.setterIn(stateGetter.owner)

    def selectResult = applyNilAfterUncurry(memberRef(nme.result))
  }

  private lazy val NonFatalClass = rootMirror.staticModule("scala.util.control.NonFatal")
  private lazy val ThrowableClass = rootMirror.staticClass("java.lang.Throwable")

  /**
   * Uses `AsyncBlockBuilder` to create an instance of `AsyncBlock`.
   *
   * @param  block      a `Block` tree in ANF
   * @param  symLookup  helper for looking up members of the state machine class
   * @return            an `AsyncBlock`
   */
  def buildAsyncBlock(block: Block, symLookup: SymLookup): AsyncBlock = {
    val Block(stats, expr) = block
    val startState = stateAssigner.nextState()
    val endState = Int.MaxValue

    val blockBuilder = new AsyncBlockBuilder(stats, expr, startState, endState, symLookup)

    new AsyncBlock {
      val switchIds = mutable.AnyRefMap[Integer, Integer]()

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
              builder.append(line.replaceAllLiterally("\"", "&quot;").replaceAllLiterally("<", "&lt;").replaceAllLiterally(">", "&gt;").replaceAllLiterally(" ", "&nbsp;"))
          }
          builder.append(br)
          builder.append("</font>")
        }
        val dotBuilder = new StringBuilder()
        dotBuilder.append("digraph {\n")
        def stateLabel(s: Int) = {
          if (s == 0) "INITIAL" else if (s == Int.MaxValue) "TERMINAL" else switchIds.getOrElse[Integer](s, s).toString
        }
        val length = states.size
        for ((state, i) <- asyncStates.zipWithIndex) {
          dotBuilder.append(s"""${stateLabel(state.state)} [label=""").append("<")
          def show(t: Tree): String = {
            (t match {
              case Block(stats, expr) => stats ::: expr :: Nil
              case t => t :: Nil
            }).iterator.map(t => showCode(t)).mkString("\n")
          }
          if (i != length - 1) {
            val CaseDef(_, _, body) = state.mkHandlerCaseForState
            toHtmlLabel(stateLabel(state.state), show(compactStateTransform.transform(body)), dotBuilder)
          } else {
            toHtmlLabel(stateLabel(state.state), state.allStats.map(show(_)).mkString("\n"), dotBuilder)
          }
          dotBuilder.append("> ]\n")
          state match {
            case s: AsyncStateWithAwait =>
              val CaseDef(_, _, body) = s.mkOnCompleteHandler.get
              dotBuilder.append(s"""${stateLabel(s.onCompleteState)} [label=""").append("<")
              toHtmlLabel(stateLabel(s.onCompleteState), show(compactStateTransform.transform(body)), dotBuilder)
              dotBuilder.append("> ]\n")
            case _ =>
          }
        }
        for (state <- states) {
          state match {
            case s: AsyncStateWithAwait =>
              dotBuilder.append(s"""${stateLabel(state.state)} -> ${stateLabel(s.onCompleteState)} [style=dashed color=red]""")
              dotBuilder.append("\n")
              for (succ <- state.nextStates) {
                dotBuilder.append(s"""${stateLabel(s.onCompleteState)} -> ${stateLabel(succ)}""")
                dotBuilder.append("\n")
              }
            case _ =>
              for (succ <- state.nextStates) {
                dotBuilder.append(s"""${stateLabel(state.state)} -> ${stateLabel(succ)}""")
                dotBuilder.append("\n")
              }
          }
        }
        dotBuilder.append("}\n")
        dotBuilder.toString
      }

      lazy val asyncStates: List[AsyncState] = filterStates

      def filterStates = {
        val all = blockBuilder.asyncStates.toList
        val (initial :: rest) = all
        val map = all.iterator.map(x => (x.state, x)).toMap
        val seen = mutable.HashSet[Int]()
        def loop(state: AsyncState): Unit = {
          seen.add(state.state)
          for (i <- state.nextStates) {
            if (i != Int.MaxValue && !seen.contains(i)) {
              loop(map(i))
            }
          }
        }
        loop(initial)
        val live = rest.filter(state => seen(state.state))
        var nextSwitchId = 0
        (initial :: live).foreach { state =>
          val switchId = nextSwitchId
          switchIds(state.state) = switchId
          nextSwitchId += 1
          state match {
            case state: AsyncStateWithAwait =>
              val switchId = nextSwitchId
              switchIds(state.onCompleteState) = switchId
              nextSwitchId += 1
            case _ =>
          }
        }
        initial :: live

      }

      def mkCombinedHandlerCases[T]: List[CaseDef] = {
        val futureSystem = currentTransformState.futureSystem
        val futureSystemOps = futureSystem.mkOps(global)

        val caseForLastState: CaseDef = {
          val lastState = asyncStates.last
          val lastStateBody = lastState.body
          val rhs = futureSystemOps.completeWithSuccess(
            symLookup.selectResult, lastStateBody)
          mkHandlerCase(lastState.state, Block(rhs, Return(literalUnit)))
        }
        asyncStates match {
          case s :: Nil =>
            List(caseForLastState)
          case _        =>
            val initCases = for (state <- asyncStates.init) yield state.mkHandlerCaseForState[T]
            initCases :+ caseForLastState
        }
      }

      val initStates = asyncStates.init

      /**
       * Builds the definition of the `resume` method.
       *
       * The resulting tree has the following shape:
       *
       *       try {
       *         state match {
       *           case 0 => {
       *             f11 = exprReturningFuture
       *             f11.onComplete(onCompleteHandler)(context)
       *           }
       *           ...
       *         }
       *       } catch {
       *         case NonFatal(t) => result.failure(t)
       *       }
       */
      private def resumeFunTree[T]: Tree = {
        val futureSystem = currentTransformState.futureSystem
        val futureSystemOps = futureSystem.mkOps(global)

        val stateMemberRef = gen.mkApplyIfNeeded(symLookup.memberRef(symLookup.stateGetter))
        val body =
          Match(stateMemberRef,
                 mkCombinedHandlerCases[T] ++
                 initStates.flatMap(_.mkOnCompleteHandler[T]) ++
                 List(CaseDef(Ident(nme.WILDCARD), EmptyTree, Throw(Apply(Select(New(Ident(IllegalStateExceptionClass)), termNames.CONSTRUCTOR), List())))))

        val body1 = compactStates(body)

        maybeTry(currentTransformState.futureSystem.emitTryCatch)(
          body1,
          List(
            CaseDef(
            Bind(nme.t, Typed(Ident(nme.WILDCARD), Ident(ThrowableClass))),
            EmptyTree, {
              val branchTrue = {
                val t = Ident(nme.t)
                val complete = futureSystemOps.completeProm[T](
                  symLookup.selectResult, futureSystemOps.tryyFailure[T](t))
                Block(toList(complete), Return(literalUnit))
              }
              If(Apply(Ident(NonFatalClass), List(Ident(nme.t))), branchTrue, Throw(Ident(nme.t)))
                branchTrue
            })), EmptyTree)
      }

      private val compactStateTransform = new Transformer {
        override def transform(tree: Tree): Tree = tree match {
          case as @ Apply(qual: Select, Literal(Constant(i: Integer)) :: Nil) if qual.symbol == symLookup.stateSetter =>
            val replacement = switchIds(i)
            treeCopy.Apply(tree, qual, Literal(Constant(replacement)):: Nil)
          case _: Match | _: CaseDef | _: Block | _: If =>
            super.transform(tree)
          case _ => tree
        }
      }

      private def compactStates(m: Match): Tree = {
        val cases1 = m.cases.flatMap {
          case cd @ CaseDef(Literal(Constant(i: Integer)), EmptyTree, rhs) =>
            val replacement = switchIds(i)
            val rhs1 = compactStateTransform.transform(rhs)
            treeCopy.CaseDef(cd, Literal(Constant(replacement)), EmptyTree, rhs1) :: Nil
          case x => x :: Nil
        }
        treeCopy.Match(m, m.selector, cases1)
      }

      def forever(t: Tree): Tree = {
        val labelName = TermName(name.fresh("while$"))
        LabelDef(labelName, Nil, Block(toList(t), Apply(Ident(labelName), Nil)))
      }

      /**
       * Builds a `match` expression used as an onComplete handler, wrapped in a while(true) loop.
       */
      def onCompleteHandler[T]: Tree = {
        forever {
          adaptToUnit(toList(resumeFunTree))
        }
      }
    }
  }

  private def isSyntheticBindVal(tree: Tree) = tree match {
    case vd@ValDef(_, lname, _, Ident(rname)) => vd.symbol.attachments.contains[SyntheticBindVal.type]
    case _                                    => false
  }

  case class Awaitable(expr: Tree, resultName: Symbol, resultType: Type, resultValDef: ValDef)

  private def mkStateTree(nextState: Int, symLookup: SymLookup): Tree =
    Apply(symLookup.memberRef(symLookup.stateSetter), Literal(Constant(nextState)) :: Nil)

  private def mkHandlerCase(num: Int, rhs: List[Tree]): CaseDef =
    mkHandlerCase(num, adaptToUnit(rhs))

  // We use the convention that the state machine's ID for a state corresponding to
  // a labeldef will a negative number be based on the symbol ID. This allows us
  // to translate a forward jump to the label as a state transition to a known state
  // ID, even though the state machine transform hasn't yet processed the target label
  // def. Negative numbers are used so as as not to clash with regular state IDs, which
  // are allocated in ascending order from 0.
  private def stateIdForLabel(sym: Symbol): Int = -sym.id

  private def mkHandlerCase(num: Int, rhs: Tree): CaseDef =
    CaseDef(Literal(Constant(num)), EmptyTree, rhs)

  // TODO AM: should this explode blocks even when expr is not ()?
  private def toList(tree: Tree): List[Tree] = tree match {
    case Block(stats, expr) if isLiteralUnit(expr) => stats
    case _ => tree :: Nil
  }

}
