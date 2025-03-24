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

import scala.annotation._
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.reflect.internal.Flags._

trait LiveVariables extends ExprBuilder {
  import global._

  /**
   *  Live variables data-flow analysis.
   *
   *  Find, for each lifted field, the last state where the field is used.
   *
   *  @param   asyncStates the states of an `async` block
   *  @param   liftables   the lifted fields
   *  @return              a map which indicates fields which are used for the final time in each state.
   */
  def fieldsToNullOut(asyncStates: List[AsyncState], @unused finalState: AsyncState,
                      liftables: List[Tree]): mutable.LinkedHashMap[Int, (mutable.LinkedHashSet[Symbol], mutable.LinkedHashSet[Symbol])] = {

    val liftedSyms = mutable.LinkedHashSet[Symbol]()

    // include only vars
    liftedSyms ++= liftables.iterator.collect {
      case vd : ValDef if vd.symbol.hasFlag(MUTABLE) =>
        vd.symbol
    }

    // determine which fields should be live also at the end (will not be nulled out)
    liftedSyms.foreach { sym =>
      val tpSym = sym.info.typeSymbol
      if ((tpSym.isPrimitiveValueClass || tpSym == definitions.NothingClass) || liftables.exists { tree =>
        !liftedSyms.contains(tree.symbol) && tree.exists(_.symbol == sym)})
        liftedSyms -= sym
    }

    /*
     *  Traverse statements of an `AsyncState`, collect `Ident`-s referring to lifted fields.
     *
     *  @param  as  a state of an `async` expression
     *  @return     a set of lifted fields that are used within state `as`
     */
    def fieldsUsedIn(as: AsyncState): (collection.Set[Symbol], collection.Set[Symbol]) = {
      class FindUseTraverser extends AsyncTraverser {
        val usedBeforeAssignment = new mutable.LinkedHashSet[Symbol]()
        val assignedFields = new mutable.LinkedHashSet[Symbol]()
        private def capturing[A](body: => A): A = {
          val saved = capturing
          try {
            capturing = true
            body
          } finally capturing = saved
        }
        private def capturingCheck(tree: Tree) = capturing(super[Traverser].traverse(tree))
        private var capturing: Boolean = false
        override def traverse(tree: Tree) = tree match {
          case Assign(i @ Ident(_), rhs) if liftedSyms(tree.symbol) =>
            if (!capturing)
              assignedFields += i.symbol
            traverse(rhs)
          case ValDef(_, _, _, rhs) if liftedSyms(tree.symbol) =>
            if (!capturing)
              assignedFields += tree.symbol
            traverse(rhs)
          case Ident(_) if liftedSyms(tree.symbol) =>
            if (capturing) {
              liftedSyms -= tree.symbol
            } else if (!assignedFields.contains(tree.symbol)) {
              usedBeforeAssignment += tree.symbol
            }
          case _ =>
            super.traverse(tree)
        }

        override def nestedClass(classDef: ClassDef): Unit = capturingCheck(classDef)

        override def nestedModuleClass(moduleClass: ClassDef): Unit = capturingCheck(moduleClass)

        override def nestedMethod(defdef: DefDef): Unit = capturingCheck(defdef)

        override def synchronizedCall(arg: Tree): Unit = capturingCheck(arg)

        override def function(function: Function): Unit = capturingCheck(function)
        override def function(expandedFunction: ClassDef): Unit = capturingCheck(expandedFunction)
      }

      val findUses = new FindUseTraverser
      findUses.traverse(Block(as.stats: _*))
      (findUses.usedBeforeAssignment, findUses.assignedFields)
    }
    val graph: Graph[AsyncState] = {
      val g = new Graph[AsyncState]
      val stateIdToState = asyncStates.iterator.map(x => (x.state, x)).toMap
      for (asyncState <- asyncStates) {
        val (used, assigned) = fieldsUsedIn(asyncState)
        g.addNode(asyncState, used, assigned, asyncState.nextStates.map(stateIdToState).toList)
      }
      g.finish()
    }

    graph.lastReferences[Int](ArraySeq.unsafeWrapArray(liftedSyms.toArray[Symbol]))(_.t.state)
  }

  private final class Graph[T] {
    import java.util.BitSet
    private val nodes = mutable.LinkedHashMap[T, Node]()
    private class Node(val t: T, val refs: collection.Set[Symbol], val assign: collection.Set[Symbol], val succTs: List[T]) {
      val succ = new Array[Node](succTs.size)
      val pred = new mutable.ArrayBuffer[Node](4)
      // Live variables at node entry
      val entry: BitSet = new BitSet
      // Live variables at node exit
      var exit = new BitSet
      // Variables generated at this node
      val gen = new BitSet
      val kill = new BitSet
      var visited: Boolean = false

      def updateEntry(): Boolean = {
        val card = entry.cardinality()
        entry.clear()
        entry.or(exit)
        entry.andNot(kill)
        entry.or(gen)
        if (!visited) {
          visited = true
          true
        } else (entry.cardinality() != card)
      }
      def updateExit(): Boolean = {
        var changed = false
        if (exit == null) {
          changed = true
          exit = new BitSet()
        }
        var i = 0
        val card = exit.cardinality()
        while (i < succ.length) {
          exit.or(succ(i).entry)
          i += 1
        }
        card != exit.cardinality()
      }
      def deadOnEntryLiveOnPredecessorExit: BitSet = {
        val result = new BitSet
        if (!pred.isEmpty) {
          val it = pred.iterator
          while (it.hasNext) {
            val pred = it.next()
            result.or(pred.exit)
          }
          result.andNot(entry)
        }
        result
      }
      def deadOnExitLiveOnEntry: BitSet = {
        val result = entry.clone.asInstanceOf[BitSet]
        result.andNot(exit)
        result
      }
      override def toString = s"Node($t, gen = $gen, kill = $kill, entry = $entry, exit = $exit, null = $deadOnEntryLiveOnPredecessorExit)"
    }
    def addNode(t: T, refs: collection.Set[Symbol], assign: collection.Set[Symbol], succ: List[T]): Unit = {
      nodes(t) = new Node(t, refs, assign, succ)
    }
    private var finished = false
    def finish(): this.type = {
      assert(!finished, "cannot finish when already finished")
      for (node <- nodes.valuesIterator) {
        foreachWithIndex(node.succTs) {(succT, i) =>
          val succ = nodes(succT)
          node.succ(i) = succ
          succ.pred += node
        }
      }
      finished = true
      this
    }
    def lastReferences[K](syms: IndexedSeq[Symbol])(keyMapping: Node => K): mutable.LinkedHashMap[K, (mutable.LinkedHashSet[Symbol], mutable.LinkedHashSet[Symbol])] = {
      assert(finished, "lastReferences before finished")
      val symIndices: Map[Symbol, Int] = syms.zipWithIndex.toMap
      val nodeValues = nodes.values.toArray
      nodeValues.foreach { node =>
        for (ref <- node.refs) {
          symIndices.getOrElse(ref, -1) match {
            case -1 =>
            case n => node.gen.set(n)
          }
        }
        for (ref <- node.assign) {
          symIndices.getOrElse(ref, -1) match {
            case -1 =>
            case n => node.kill.set(n)
          }
        }
      }
      val terminal = nodeValues.last
      val workList = mutable.Queue[Node](terminal)
      while (!workList.isEmpty) {
        val node = workList.dequeue()
        node.updateExit()
        val entryChanged = node.updateEntry()
        if (entryChanged) {
          workList ++= node.pred
        }
      }
      val empty = mutable.LinkedHashSet[Symbol]()
      def toSymSet(indices: BitSet): mutable.LinkedHashSet[Symbol] = {
        if (indices.isEmpty) empty
        else {
          val result = mutable.LinkedHashSet[Symbol]()
          indices.stream().forEach(i => result += syms(i))
          result
        }
      }
      mutable.LinkedHashMap(ArraySeq.unsafeWrapArray(nodeValues.map { x =>
        val pre = toSymSet(x.deadOnEntryLiveOnPredecessorExit)
        val post = toSymSet(x.deadOnExitLiveOnEntry)
        (keyMapping(x), (pre, post))
      }): _*)
    }
  }
}
