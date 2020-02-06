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

import java.util.function.IntConsumer

import scala.collection.immutable.IntMap
import scala.collection.mutable

import scala.reflect.internal.Flags._

trait LiveVariables extends ExprBuilder {
  import global._

  /**
   *  Returns for a given state a list of fields (as trees) that should be nulled out
   *  upon resuming that state (at the beginning of `resume`).
   *
   *  @param   asyncStates the states of an `async` block
   *  @param   liftables   the lifted fields
   *  @return  a map mapping a state to the fields that should be nulled out
   *           upon resuming that state
   */
  def fieldsToNullOut(asyncStates: List[AsyncState], liftables: List[Tree]): mutable.LinkedHashMap[Int, List[Tree]] = {
    // live variables analysis:
    // the result map indicates in which states a given field should be nulled out
    val liveVarsMap: mutable.LinkedHashMap[Tree, StateSet] = liveVars(asyncStates, liftables)

    val assignsOf = mutable.LinkedHashMap[Int, List[Tree]]()

    for ((fld, where) <- liveVarsMap) {
      where.foreach { new IntConsumer { def accept(state: Int): Unit = {
        assignsOf get state match {
          case None =>
            assignsOf += (state -> List(fld))
          case Some(trees) if !trees.exists(_.symbol == fld.symbol) =>
            assignsOf += (state -> (fld +: trees))
          case _ =>
            // do nothing
        }
      }}}
    }

    assignsOf
  }

  /**
   *  Live variables data-flow analysis.
   *
   *  The goal is to find, for each lifted field, the last state where the field is used.
   *  In all direct successor states which are not (indirect) predecessors of that last state
   *  (possible through loops), the corresponding field should be nulled out (at the beginning of
   *  `resume`).
   *
   *  @param   asyncStates the states of an `async` block
   *  @param   liftables   the lifted fields
   *  @return              a map which indicates for a given field (the key) the states in which it should be nulled out
   */
  def liveVars(asyncStates: List[AsyncState], liftables: List[Tree]): mutable.LinkedHashMap[Tree, StateSet] = {
    val liftedSyms: Set[Symbol] = // include only vars
      liftables.iterator.filter {
        case ValDef(mods, _, _, _) => mods.hasFlag(MUTABLE)
        case _ => false
      }.map(_.symbol).toSet

    // determine which fields should be live also at the end (will not be nulled out)
    val noNull: Set[Symbol] = liftedSyms.filter { sym =>
      val tpSym = sym.info.typeSymbol
      (tpSym.isClass && (tpSym.asClass.isPrimitive || isNothingClass(tpSym))) || liftables.exists { tree =>
        !liftedSyms.contains(tree.symbol) && tree.exists(_.symbol == sym)
      }
    }
    AsyncUtils.vprintln(s"fields never zero-ed out: ${noNull.mkString(", ")}")

    /**
     *  Traverse statements of an `AsyncState`, collect `Ident`-s referring to lifted fields.
     *
     *  @param  as  a state of an `async` expression
     *  @return     a set of lifted fields that are used within state `as`
     */
    def fieldsUsedIn(as: AsyncState): ReferencedFields = {
      class FindUseTraverser extends AsyncTraverser {
        var usedFields: Set[Symbol] = Set[Symbol]()
        var capturedFields: Set[Symbol] = Set[Symbol]()
        private def capturing[A](body: => A): A = {
          val saved = capturing
          try {
            capturing = true
            body
          } finally capturing = saved
        }
        private def capturingCheck(tree: Tree) = capturing(tree foreach check)
        private var capturing: Boolean = false
        private def check(tree: Tree): Unit = {
          tree match {
            case Ident(_) if liftedSyms(tree.symbol) =>
              if (capturing)
                capturedFields += tree.symbol
              else
                usedFields += tree.symbol
            case _ =>
          }
        }
        override def traverse(tree: Tree) = {
          check(tree)
          super.traverse(tree)
        }

        override def nestedClass(classDef: ClassDef): Unit = capturingCheck(classDef)

        override def nestedModule(module: ModuleDef): Unit = capturingCheck(module)

        override def nestedMethod(defdef: DefDef): Unit = capturingCheck(defdef)

        override def byNameArgument(arg: Tree): Unit = capturingCheck(arg)

        override def function(function: Function): Unit = capturingCheck(function)

        override def patMatFunction(tree: Match): Unit = capturingCheck(tree)
      }

      val findUses = new FindUseTraverser
      findUses.traverse(Block(as.stats: _*))
      ReferencedFields(findUses.usedFields, findUses.capturedFields)
    }
    case class ReferencedFields(used: Set[Symbol], captured: Set[Symbol]) {
      override def toString = s"used: ${used.mkString(",")}\ncaptured: ${captured.mkString(",")}"
    }

    /* Build the control-flow graph.
     *
     * A state `i` is contained in the list that is the value to which
     * key `j` maps iff control can flow from state `j` to state `i`.
     */
    val cfg: Map[Int, Array[Int]] = {
      var res = IntMap.empty[Array[Int]]

      for (as <- asyncStates) res = res.updated(as.state, as.nextStates)
      res
    }

    /** Tests if `state1` is a predecessor of `state2`.
     */
    def isPred(state1: Int, state2: Int): Boolean = {
      val seen = new StateSet()

      def isPred0(state1: Int, state2: Int): Boolean =
        if(state1 == state2) false
        else if (seen.contains(state1)) false  // breaks cycles in the CFG
        else cfg get state1 match {
          case Some(nextStates) =>
            seen += state1
            var i = 0
            while (i < nextStates.length) {
              if (nextStates(i) == state2 || isPred0(nextStates(i), state2)) return true
              i += 1
            }
            false
          case None =>
            false
        }

      isPred0(state1, state2)
    }

    val finalState = asyncStates.find(as => !asyncStates.exists(other => isPred(as.state, other.state))).get

    if(AsyncUtils.verbose) {
      for (as <- asyncStates)
        AsyncUtils.vprintln(s"fields used in state #${as.state}: ${fieldsUsedIn(as)}")
    }

    /* Backwards data-flow analysis. Computes live variables information at entry and exit
     * of each async state.
     *
     * Compute using a simple fixed point iteration:
     *
     * 1. currStates = List(finalState)
     * 2. for each cs \in currStates, compute LVentry(cs) from LVexit(cs) and used fields information for cs
     * 3. record if LVentry(cs) has changed for some cs.
     * 4. obtain predecessors pred of each cs \in currStates
     * 5. for each p \in pred, compute LVexit(p) as union of the LVentry of its successors
     * 6. currStates = pred
     * 7. repeat if something has changed
     */

    var LVentry = IntMap[Set[Symbol]]() withDefaultValue Set[Symbol]()
    var LVexit  = IntMap[Set[Symbol]]() withDefaultValue Set[Symbol]()

    // All fields are declared to be dead at the exit of the final async state, except for the ones
    // that cannot be nulled out at all (those in noNull), because they have been captured by a nested def.
    LVexit = LVexit + (finalState.state -> noNull)

    var currStates = List(finalState)    // start at final state
    var captured: Set[Symbol] = Set()

    def contains(as: Array[Int], a: Int): Boolean = {
      var i = 0
      while (i < as.length) {
        if (as(i) == a) return true
        i += 1
      }
      false
    }
    while (!currStates.isEmpty) {
      var entryChanged: List[AsyncState] = Nil

      for (cs <- currStates) {
        val LVentryOld = LVentry(cs.state)
        val referenced = fieldsUsedIn(cs)
        captured ++= referenced.captured
        val LVentryNew = LVexit(cs.state) ++ referenced.used
        if (!LVentryNew.sameElements(LVentryOld)) {
          LVentry = LVentry.updated(cs.state, LVentryNew)
          entryChanged ::= cs
        }
      }

      val pred = entryChanged.flatMap(cs => asyncStates.filter(state => contains(state.nextStates, cs.state)))
      var exitChanged: List[AsyncState] = Nil

      for (p <- pred) {
        val LVexitOld = LVexit(p.state)
        val LVexitNew = p.nextStates.flatMap(succ => LVentry(succ)).toSet
        if (!LVexitNew.sameElements(LVexitOld)) {
          LVexit = LVexit.updated(p.state, LVexitNew)
          exitChanged ::= p
        }
      }

      currStates = exitChanged
    }

    if(AsyncUtils.verbose) {
      for (as <- asyncStates) {
        AsyncUtils.vprintln(s"LVentry at state #${as.state}: ${LVentry(as.state).mkString(", ")}")
        AsyncUtils.vprintln(s"LVexit  at state #${as.state}: ${LVexit(as.state).mkString(", ")}")
      }
    }

    def lastUsagesOf(field: Tree, at: AsyncState): StateSet = {
      val avoid = scala.collection.mutable.HashSet[AsyncState]()

      val result = new StateSet
      def lastUsagesOf0(field: Tree, at: AsyncState): Unit = {
        if (avoid(at)) ()
        else if (captured(field.symbol)) {
          ()
        }
        else LVentry get at.state match {
          case Some(fields) if fields.contains(field.symbol) =>
            result += at.state
          case _ =>
            avoid += at
            for (state <- asyncStates) {
              if (contains(state.nextStates, at.state)) {
                lastUsagesOf0(field, state)
              }
            }
        }
      }

      lastUsagesOf0(field, at)
      result
    }

    val lastUsages: mutable.LinkedHashMap[Tree, StateSet] =
      mutable.LinkedHashMap(liftables.map(fld => fld -> lastUsagesOf(fld, finalState)): _*)

    if(AsyncUtils.verbose) {
      for ((fld, lastStates) <- lastUsages)
        AsyncUtils.vprintln(s"field ${fld.symbol.name} is last used in states ${lastStates.iterator.mkString(", ")}")
    }

    val nullOutAt: mutable.LinkedHashMap[Tree, StateSet] =
      for ((fld, lastStates) <- lastUsages) yield {
        val result = new StateSet
        lastStates.foreach(new IntConsumer { def accept(s: Int): Unit = {
          if (s != finalState.state) {
            val lastAsyncState = asyncStates.find(_.state == s).get
            val succNums       = lastAsyncState.nextStates
            // all successor states that are not indirect predecessors
            // filter out successor states where the field is live at the entry
            var i = 0
            while (i < succNums.length) {
              val num = succNums(i)
              if (!isPred(num, s) && !LVentry(num).contains(fld.symbol))
                result += num
              i += 1
            }
          }
        }})
        (fld, result)
      }

    if(AsyncUtils.verbose) {
      for ((fld, killAt) <- nullOutAt)
        AsyncUtils.vprintln(s"field ${fld.symbol.name} should be nulled out in states ${killAt.iterator.mkString(", ")}")
    }

    nullOutAt
  }
}
