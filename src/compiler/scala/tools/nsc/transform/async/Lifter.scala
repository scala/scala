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

import scala.collection.{mutable, Seq => CSeq}
import scala.reflect.internal.Flags._

trait Lifter extends ExprBuilder {
  import global._

  /**
   * Identify which DefTrees are used (including transitively) which are declared
   * in some state but used (including transitively) in another state.
   *
   * These will need to be lifted to class members of the state machine.
   */
  def liftables(asyncStates: List[AsyncState]): List[Tree] = {
    object companionship {
      private val companions = collection.mutable.Map[Symbol, Symbol]()

      def record(classes: CSeq[Symbol], moduleClasses: CSeq[Symbol]): Unit = {
        // Keep note of local companions so we rename them consistently when lifting.
        for {
          cd <- classes
          md <- moduleClasses
          if cd.name == md.name
        } {
          companions(cd) = md
          companions(md) = cd
        }
      }
      def companionOf(sym: Symbol): Symbol = {
        companions.getOrElse(sym, NoSymbol)
      }
    }


    val defs: mutable.LinkedHashMap[Tree, Int] = {
      /** Collect the DefTrees directly enclosed within `t` that have the same owner */
      object traverser extends Traverser {
        val childDefs = mutable.ArrayBuffer[Tree]()
        private val classesBuffer, moduleClassesBuffer = mutable.ArrayBuffer[Symbol]()
        override def traverse(tree: Tree): Unit = tree match {
          case _: LabelDef => super.traverse(tree)
          case _: TypeDef =>
            abort("Unexpected tree. TypeDefs should have been eliminated after erasure" + tree)
          case _: DefTree => childDefs += tree
          case _: Function =>
          case Block(stats, expr) =>
            classesBuffer.clear()
            moduleClassesBuffer.clear()
            for (stat <- stats) {
              stat match {
                case _: ClassDef =>
                  val sym = stat.symbol
                  val buffer = if (sym.isModuleClass) moduleClassesBuffer else classesBuffer
                  buffer += sym
                case _ =>
              }
            }
            companionship.record(classesBuffer, moduleClassesBuffer)
            assert(!expr.isInstanceOf[ClassDef], "expression cannot be a class def")
            super.traverse(tree)
          case _ =>
            super.traverse(tree)
        }
      }

      val result = mutable.LinkedHashMap[Tree, Int]()

      for (asyncState <- asyncStates) {
        traverser.childDefs.clear()
        traverser.traverse(Block(asyncState.stats, EmptyTree))
        for (defTree <-traverser.childDefs) {
          result(defTree) = asyncState.state
        }
      }
      result
    }

    // In which block are these symbols defined?
    val symToDefiningState: mutable.LinkedHashMap[Symbol, Int] = defs.map {
      case (k, v) => (k.symbol, v)
    }

    // The definitions trees
    val symToTree: mutable.LinkedHashMap[Symbol, Tree] = defs.map {
      case (k, _) => (k.symbol, k)
    }

    // The direct references of each definition tree
    val defSymToReferenced: mutable.LinkedHashMap[Symbol, List[Symbol]] = defs.map {
      case (tree, _) => (tree.symbol, tree.collect {
        case rt: RefTree if symToDefiningState.contains(rt.symbol) => rt.symbol
      } ::: tree.symbol.info.collect { case TypeRef(_, sym, _) if symToDefiningState.contains(sym) => sym })
    }

    // The direct references of each block, excluding references of `DefTree`-s which
    // are already accounted for.
    val stateIdToDirectlyReferenced: mutable.LinkedHashMap[Int, List[Symbol]] = {
      val refs: List[(Int, Symbol)] = asyncStates.flatMap(
        asyncState => asyncState.stats.filterNot(t => t.isDef && !isLabel(t.symbol)).flatMap(_.collect {
          case rt: RefTree
            if symToDefiningState.contains(rt.symbol) => (asyncState.state, rt.symbol)
        })
      )
      toMultiMap(refs)
    }

    def liftableSyms: mutable.LinkedHashSet[Symbol] = {
      val liftableMutableSet = mutable.LinkedHashSet[Symbol]()
      def markForLift(sym: Symbol): Unit = {
        if (!liftableMutableSet(sym)) {
          liftableMutableSet += sym

          // Only mark transitive references of defs, modules and classes. The RHS of lifted vals/vars
          // stays in its original location, so things that it refers to need not be lifted.
          if (!(sym.isTerm && (sym.asTerm.isVal || sym.asTerm.isVar)))
            defSymToReferenced(sym).foreach(sym2 => markForLift(sym2))
        }
      }
      // Start things with DefTrees directly referenced from statements from other states...
      val liftableStatementRefs: List[Symbol] = stateIdToDirectlyReferenced.iterator.flatMap {
        case (i, syms) => syms.filter(sym => symToDefiningState(sym) != i)
      }.toList
      // .. and likewise for DefTrees directly referenced by other DefTrees from other states
      val liftableRefsOfDefTrees = defSymToReferenced.toList.flatMap {
        case (referee, referents) => referents.filter(sym => symToDefiningState(sym) != symToDefiningState(referee))
      }
      // Mark these for lifting, which will follow transitive references.
      (liftableStatementRefs ++ liftableRefsOfDefTrees).foreach(markForLift)
      liftableMutableSet
    }
    val changer = new ChangeOwnerTraverser(currentTransformState.applySym, currentTransformState.stateMachineClass)
    liftableSyms.iterator.map(symToTree).map {
      t =>
        val sym = t.symbol
        changer.change(sym)

        val treeLifted = t match {
          case vd@ValDef(_, _, tpt, rhs)                    =>
            val isLazy = sym.isLazy
            sym.setFlag(STABLE)
            if (currentTransformState.exteralFsmSelfParam == NoSymbol)
              sym.setFlag(PRIVATE | LOCAL)

            if (isLazy) sym.resetFlag(LAZY)
            sym.setFlag(MUTABLE)
            sym.setName(currentTransformState.name.freshenIfNeeded(sym.name.toTermName))
            sym.setInfo(sym.info.deconst)
            treeCopy.ValDef(vd, Modifiers(sym.flags), sym.name, TypeTree(sym.info).setPos(t.pos), EmptyTree)
          case dd@DefDef(_, _, tparams, vparamss, tpt, rhs) =>
            sym.setName(currentTransformState.name.freshen(sym.name.toTermName))
            if (currentTransformState.exteralFsmSelfParam == NoSymbol)
              sym.setFlag(PRIVATE | LOCAL)
            // Was `DefDef(sym, rhs)`, but this ran afoul of `ToughTypeSpec.nestedMethodWithInconsistencyTreeAndInfoParamSymbols`
            // due to the handling of type parameter skolems in `thisMethodType` in `Namers`
            treeCopy.DefDef(dd, Modifiers(sym.flags), sym.name, tparams, vparamss, tpt, rhs)
          case cd@ClassDef(_, _, tparams, impl)             =>
            val companion = companionship.companionOf(cd.symbol)
            if (!cd.symbol.isModuleClass) {
              sym.setName(currentTransformState.name.freshen(sym.name.toTypeName))
              companion match {
                case NoSymbol =>
                case moduleClassSymbol =>
                  moduleClassSymbol.setName(sym.name.toTypeName)
                  // TODO rename the other lazy artifacts? Foo$lazy
              }
              treeCopy.ClassDef(cd, Modifiers(sym.flags), sym.name, tparams, impl)
            } else {
              companion match {
                case NoSymbol    =>
                  sym.setName(currentTransformState.name.freshen(sym.name.toTypeName))
                  sym.setName(sym.name.toTypeName)
                case classSymbol@_ => // will be renamed by above.
              }
              treeCopy.ClassDef(cd, Modifiers(sym.flags), sym.name, tparams, impl)
            }
          case x => throw new MatchError(x)
        }
        atPos(t.pos)(treeLifted)
    }.toList
  }
}
