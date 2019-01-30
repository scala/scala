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
import scala.reflect.internal.Flags._

trait Lifter extends ExprBuilder {
  import u._

  /**
   * Identify which DefTrees are used (including transitively) which are declared
   * in some state but used (including transitively) in another state.
   *
   * These will need to be lifted to class members of the state machine.
   */
  def liftables(asyncStates: List[AsyncState]): List[Tree] = {
    object companionship {
      private val companions = collection.mutable.Map[Symbol, Symbol]()
      private val companionsInverse = collection.mutable.Map[Symbol, Symbol]()
      private def record(sym1: Symbol, sym2: Symbol): Unit = {
        companions(sym1) = sym2
        companions(sym2) = sym1
      }

      def record(defs: List[Tree]): Unit = {
        // Keep note of local companions so we rename them consistently
        // when lifting.
        for {
          cd@ClassDef(_, _, _, _) <- defs
          md@ModuleDef(_, _, _) <- defs
          if (cd.name.toTermName == md.name)
        } record(cd.symbol, md.symbol)
      }
      def companionOf(sym: Symbol): Symbol = {
        companions.get(sym).orElse(companionsInverse.get(sym)).getOrElse(NoSymbol)
      }
    }


    val defs: mutable.LinkedHashMap[Tree, Int] = {
      /** Collect the DefTrees directly enclosed within `t` that have the same owner */
      def collectDirectlyEnclosedDefs(t: Tree): List[DefTree] = t match {
        case ld: LabelDef => Nil
        case dt: DefTree => dt :: Nil
        case _: Function => Nil
        case t           =>
          val childDefs = t.children.flatMap(collectDirectlyEnclosedDefs(_))
          companionship.record(childDefs)
          childDefs
      }
      mutable.LinkedHashMap(asyncStates.flatMap {
        asyncState =>
          val defs = collectDirectlyEnclosedDefs(Block(asyncState.allStats: _*))
          defs.map((_, asyncState.state))
      }: _*)
    }

    // In which block are these symbols defined?
    val symToDefiningState: mutable.LinkedHashMap[Symbol, Int] = defs.map {
      case (k, v) => (k.symbol, v)
    }

    // The definitions trees
    val symToTree: mutable.LinkedHashMap[Symbol, Tree] = defs.map {
      case (k, v) => (k.symbol, k)
    }

    // The direct references of each definition tree
    val defSymToReferenced: mutable.LinkedHashMap[Symbol, List[Symbol]] = defs.map {
      case (tree, _) => (tree.symbol, tree.collect {
        case rt: RefTree if symToDefiningState.contains(rt.symbol) => rt.symbol
      })
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
          if (!(sym.isTerm && !sym.asTerm.isLazy && (sym.asTerm.isVal || sym.asTerm.isVar)))
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

    liftableSyms.iterator.map(symToTree).map {
      t =>
        val sym = t.symbol
        val treeLifted = t match {
          case vd@ValDef(_, _, tpt, rhs)                    =>
            sym.setFlag(MUTABLE | STABLE | PRIVATE | LOCAL)
            sym.setName(name.fresh(sym.name.toTermName))
            sym.setInfo(sym.info.deconst)
            val rhs1 = if (sym.asTerm.isLazy) rhs else EmptyTree
            treeCopy.ValDef(vd, Modifiers(sym.flags), sym.name, TypeTree(sym.info).setPos(t.pos), rhs1)
          case dd@DefDef(_, _, tparams, vparamss, tpt, rhs) =>
            sym.setName(this.name.freshen(sym.name.toTermName))
            sym.setFlag(PRIVATE | LOCAL)
            // Was `DefDef(sym, rhs)`, but this ran afoul of `ToughTypeSpec.nestedMethodWithInconsistencyTreeAndInfoParamSymbols`
            // due to the handling of type parameter skolems in `thisMethodType` in `Namers`
            treeCopy.DefDef(dd, Modifiers(sym.flags), sym.name, tparams, vparamss, tpt, rhs)
          case cd@ClassDef(_, _, tparams, impl)             =>
            sym.setName(name.freshen(sym.name.toTypeName))
            companionship.companionOf(cd.symbol) match {
              case NoSymbol     =>
              case moduleSymbol =>
                moduleSymbol.setName(sym.name.toTermName)
                moduleSymbol.asModule.moduleClass.setName(moduleSymbol.name.toTypeName)
            }
            treeCopy.ClassDef(cd, Modifiers(sym.flags), sym.name, tparams, impl)
          case md@ModuleDef(_, _, impl)                     =>
            companionship.companionOf(md.symbol) match {
              case NoSymbol    =>
                sym.setName(name.freshen(sym.name.toTermName))
                sym.asModule.moduleClass.setName(sym.name.toTypeName)
              case classSymbol => // will be renamed by `case ClassDef` above.
            }
            treeCopy.ModuleDef(md, Modifiers(sym.flags), sym.name, impl)
          case td@TypeDef(_, _, tparams, rhs)               =>
            sym.setName(name.freshen(sym.name.toTypeName))
            treeCopy.TypeDef(td, Modifiers(sym.flags), sym.name, tparams, rhs)
        }
        atPos(t.pos)(treeLifted)
    }.toList
  }
}
