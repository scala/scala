/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

import scala.tools.nsc.symtab.Flags

import scala.collection.{ mutable, immutable }

/** Duplicate trees and re-type check them, taking care to replace
 *  and create fresh symbols for new local definitions.
 *
 *  @author  Iulian Dragos
 *  @version 1.0
 */
abstract class Duplicators extends Analyzer {
  import global._

  def retyped(context: Context, tree: Tree): Tree = {
    resetClassOwners
    (new BodyDuplicator(context)).typed(tree)
  }

  /** Retype the given tree in the given context. Use this method when retyping
   *  a method in a different class. The typer will replace references to the this of
   *  the old class with the new class, and map symbols through the given 'env'. The
   *  environment is a map from type skolems to concrete types (see SpecializedTypes).
   */
  def retyped(context: Context, tree: Tree, oldThis: Symbol, newThis: Symbol, env: collection.Map[Symbol, Type]): Tree = {
    if (oldThis ne newThis) {
      oldClassOwner = oldThis
      newClassOwner = newThis
    } else resetClassOwners

    envSubstitution = new SubstSkolemsTypeMap(env.keysIterator.toList, env.valuesIterator.toList)
    log("retyped with env: " + env)
    (new BodyDuplicator(context)).typed(tree)
  }

  def retypedMethod(context: Context, tree: Tree, oldThis: Symbol, newThis: Symbol): Tree =
    (new BodyDuplicator(context)).retypedMethod(tree.asInstanceOf[DefDef], oldThis, newThis)

  /** Return the special typer for duplicate method bodies. */
  override def newTyper(context: Context): Typer =
    new BodyDuplicator(context)

  private def resetClassOwners() {
    oldClassOwner = null
    newClassOwner = null
  }

  private var oldClassOwner: Symbol = _
  private var newClassOwner: Symbol = _
  private var envSubstitution: SubstTypeMap = _

  private class SubstSkolemsTypeMap(from: List[Symbol], to: List[Type]) extends SubstTypeMap(from, to) {
    protected override def matches(sym1: Symbol, sym2: Symbol) =
      if (sym2.isTypeSkolem) sym2.deSkolemize eq sym1
      else sym1 eq sym2
  }

  private val invalidSyms: mutable.Map[Symbol, Tree] = perRunCaches.newMap[Symbol, Tree]()

  /** A typer that creates new symbols for all definitions in the given tree
   *  and updates references to them while re-typechecking. All types in the
   *  tree, except for TypeTrees, are erased prior to type checking. TypeTrees
   *  are fixed by substituting invalid symbols for the new ones.
   */
  class BodyDuplicator(_context: Context) extends Typer(_context) {

    class FixInvalidSyms extends TypeMap {

      def apply(tpe: Type): Type = {
        mapOver(tpe)
      }

      override def mapOver(tpe: Type): Type = tpe match {
        case TypeRef(NoPrefix, sym, args) if sym.isTypeParameterOrSkolem =>
          val sym1 = context.scope.lookup(sym.name)
//          assert(sym1 ne NoSymbol, tpe)
          if ((sym1 ne NoSymbol) && (sym1 ne sym)) {
            log("fixing " + sym + " -> " + sym1)
            typeRef(NoPrefix, sym1, mapOverArgs(args, sym1.typeParams))
          } else super.mapOver(tpe)

        case TypeRef(pre, sym, args) =>
          val newsym = updateSym(sym)
          if (newsym ne sym) {
            log("fixing " + sym + " -> " + newsym)
            typeRef(mapOver(pre), newsym, mapOverArgs(args, newsym.typeParams))
          } else
            super.mapOver(tpe)

        case SingleType(pre, sym) =>
          val sym1 = updateSym(sym)
          if (sym1 ne sym) {
            log("fixing " + sym + " -> " + sym1)
            singleType(mapOver(pre), sym1)
          } else
            super.mapOver(tpe)

        case ThisType(sym) =>
          val sym1 = updateSym(sym)
          if (sym1 ne sym) {
            log("fixing " + sym + " -> " + sym1)
            ThisType(sym1)
          } else
            super.mapOver(tpe)


        case _ =>
          super.mapOver(tpe)
      }
    }

    /** Fix the given type by replacing invalid symbols with the new ones. */
    def fixType(tpe: Type): Type = {
      val tpe1 = envSubstitution(tpe)
      val tpe2: Type = (new FixInvalidSyms)(tpe1)
      val tpe3 = if (newClassOwner ne null) {
        tpe2.asSeenFrom(newClassOwner.thisType, oldClassOwner)
      } else tpe2
      tpe3
    }

    /** Return the new symbol corresponding to `sym`. */
    private def updateSym(sym: Symbol): Symbol =
      if (invalidSyms.isDefinedAt(sym))
        invalidSyms(sym).symbol
      else
        sym

    private def invalidate(tree: Tree) {
      debuglog("attempting to invalidate " + tree.symbol + ", owner - " + (if (tree.symbol ne null) tree.symbol.owner else "<NULL>"))
      if (tree.isDef && tree.symbol != NoSymbol) {
        log("invalid " + tree.symbol)
        invalidSyms(tree.symbol) = tree

        tree match {
          case ldef @ LabelDef(name, params, rhs) =>
            log("LabelDef " + name + " sym.info: " + ldef.symbol.info)
            invalidSyms(ldef.symbol) = ldef
          //          breakIf(true, this, ldef, context)
            val newsym = ldef.symbol.cloneSymbol(context.owner)
            newsym.setInfo(fixType(ldef.symbol.info))
            ldef.symbol = newsym
            log("newsym: " + newsym + " info: " + newsym.info)

          case vdef @ ValDef(mods, name, _, rhs) if mods.hasFlag(Flags.LAZY) =>
            log("ValDef " + name + " sym.info: " + vdef.symbol.info)
            invalidSyms(vdef.symbol) = vdef
            val newsym = vdef.symbol.cloneSymbol(context.owner)
            newsym.setInfo(fixType(vdef.symbol.info))
            vdef.symbol = newsym
            log("newsym: " + newsym + " info: " + newsym.info)

          case DefDef(_, name, tparams, vparamss, _, rhs) =>
            // invalidate parameters
            invalidate(tparams ::: vparamss.flatten)
            tree.symbol = NoSymbol

          case _ =>
            tree.symbol = NoSymbol
        }
      }
    }

    private def invalidate(stats: List[Tree]) {
      stats foreach invalidate
    }

    def retypedMethod(ddef: DefDef, oldThis: Symbol, newThis: Symbol): Tree = {
      oldClassOwner = oldThis
      newClassOwner = newThis
      invalidate(ddef.tparams)
      for (vdef <- ddef.vparamss.flatten) {
        invalidate(vdef)
        vdef.tpe = null
      }
      ddef.symbol = NoSymbol
      enterSym(context, ddef)
      log("remapping this of " + oldClassOwner + " to " + newClassOwner)
      typed(ddef)
    }

    private def inspectTpe(tpe: Type) = {
      tpe match {
        case MethodType(_, res) =>
          res + ", " + res.bounds.hi + ", " + (res.bounds.hi match {
            case TypeRef(_, _, args) if (args.length > 0) => args(0) + ", " + args(0).bounds.hi
            case _ => "non-tref: " + res.bounds.hi.getClass
          })
        case _ =>
      }
    }

    /** Special typer method for re-type checking trees. It expects a typed tree.
     *  Returns a typed tree that has fresh symbols for all definitions in the original tree.
     *
     *  Each definition tree is visited and its symbol added to the invalidSyms map (except LabelDefs),
     *  then cleared (forcing the namer to create fresh symbols).
     *  All invalid symbols found in trees are cleared (except for LabelDefs), forcing the
     *  typechecker to look for fresh ones in the context.
     *
     *  Type trees are typed by substituting old symbols for new ones (@see fixType).
     *
     *  LabelDefs are not typable from trees alone, unless they have the type ()Unit. Therefore,
     *  their symbols are recreated ad-hoc and their types are fixed inline, instead of letting the
     *  namer/typer handle them, or Idents that refer to them.
     */
    override def typed(tree: Tree, mode: Int, pt: Type): Tree = {
      debuglog("typing " + tree + ": " + tree.tpe + ", " + tree.getClass)
      val origtreesym = tree.symbol
      if (tree.hasSymbol && tree.symbol != NoSymbol
          && !tree.symbol.isLabel  // labels cannot be retyped by the type checker as LabelDef has no ValDef/return type trees
          && invalidSyms.isDefinedAt(tree.symbol)) {
        debuglog("removed symbol " + tree.symbol)
        tree.symbol = NoSymbol
      }

      tree match {
        case ttree @ TypeTree() =>
          // log("fixing tpe: " + tree.tpe + " with sym: " + tree.tpe.typeSymbol)
          ttree.tpe = fixType(ttree.tpe)
          ttree

        case Block(stats, res) =>
          log("invalidating block")
          invalidate(stats)
          invalidate(res)
          tree.tpe = null
          super.typed(tree, mode, pt)

        case ClassDef(_, _, _, tmpl @ Template(parents, _, stats)) =>
          // log("invalidating classdef " + tree.tpe)
          tmpl.symbol = tree.symbol.newLocalDummy(tree.pos)
          invalidate(stats)
          tree.tpe = null
          super.typed(tree, mode, pt)

        case ddef @ DefDef(_, _, _, _, tpt, rhs) =>
          ddef.tpt.tpe = fixType(ddef.tpt.tpe)
          ddef.tpe = null
          super.typed(ddef, mode, pt)

        case vdef @ ValDef(mods, name, tpt, rhs) =>
          // log("vdef fixing tpe: " + tree.tpe + " with sym: " + tree.tpe.typeSymbol + " and " + invalidSyms)
          if (mods.hasFlag(Flags.LAZY)) vdef.symbol.resetFlag(Flags.MUTABLE)
          vdef.tpt.tpe = fixType(vdef.tpt.tpe)
          vdef.tpe = null
          super.typed(vdef, mode, pt)

        case ldef @ LabelDef(name, params, rhs) =>
          // log("label def: " + ldef)
          ldef.tpe = null
          val params1 = params map { p => Ident(updateSym(p.symbol)) }
          super.typed(treeCopy.LabelDef(tree, name, params1, rhs), mode, pt)

        case Bind(name, _) =>
          // log("bind: " + tree)
          invalidate(tree)
          tree.tpe = null
          super.typed(tree, mode, pt)

        case Ident(_) if tree.symbol.isLabel =>
          log("Ident to labeldef " + tree + " switched to ")
          tree.symbol = updateSym(tree.symbol)
          tree.tpe = null
          super.typed(tree, mode, pt)

        case Ident(_) if (origtreesym ne null) && origtreesym.isLazy =>
          log("Ident to a lazy val " + tree + ", " + tree.symbol + " updated to " + origtreesym)
          tree.symbol = updateSym(origtreesym)
          tree.tpe = null
          super.typed(tree, mode, pt)

        case Select(th @ This(_), sel) if (oldClassOwner ne null) && (th.symbol == oldClassOwner) =>
          // log("selection on this, no type ascription required")
          // we use the symbol name instead of the tree name because the symbol may have been
          // name mangled, rendering the tree name obsolete
          // log(tree)
          val t = super.typed(atPos(tree.pos)(Select(This(newClassOwner), tree.symbol.name)), mode, pt)
          // log("typed to: " + t + "; tpe = " + t.tpe + "; " + inspectTpe(t.tpe))
          t

        case This(_) if (oldClassOwner ne null) && (tree.symbol == oldClassOwner) =>
//          val tree1 = Typed(This(newClassOwner), TypeTree(fixType(tree.tpe.widen)))
          // log("selection on this: " + tree)
          val tree1 = This(newClassOwner)
          // log("tree1: " + tree1)
          debuglog("mapped " + tree + " to " + tree1)
          super.typed(atPos(tree.pos)(tree1), mode, pt)

        case This(_) =>
          // log("selection on this, plain: " + tree)
          tree.symbol = updateSym(tree.symbol)
          tree.tpe = null
          val tree1 = super.typed(tree, mode, pt)
          // log("plain this typed to: " + tree1)
          tree1
/* no longer needed, because Super now contains a This(...)
        case Super(qual, mix) if (oldClassOwner ne null) && (tree.symbol == oldClassOwner) =>
          val tree1 = Super(qual, mix)
          log("changed " + tree + " to " + tree1)
          super.typed(atPos(tree.pos)(tree1))
*/
        case Match(scrut, cases) =>
          val scrut1 = typed(scrut, EXPRmode | BYVALmode, WildcardType)
          val scrutTpe = scrut1.tpe.widen
          val cases1 = if (scrutTpe.isFinalType) cases filter {
            case CaseDef(Bind(_, pat @ Typed(_, tpt)), EmptyTree, body) =>
              // the typed pattern is not incompatible with the scrutinee type
              scrutTpe.matchesPattern(fixType(tpt.tpe))
            case CaseDef(Typed(_, tpt), EmptyTree, body) =>
              // the typed pattern is not incompatible with the scrutinee type
              scrutTpe.matchesPattern(fixType(tpt.tpe))
            case _ => true
          } else cases

          super.typed(atPos(tree.pos)(Match(scrut, cases1)), mode, pt)

        case EmptyTree =>
          // no need to do anything, in particular, don't set the type to null, EmptyTree.tpe_= asserts
          tree

        case _ =>
          log("default: " + tree)
          if (tree.hasSymbol && tree.symbol != NoSymbol && (tree.symbol.owner == definitions.AnyClass)) {
            tree.symbol = NoSymbol // maybe we can find a more specific member in a subclass of Any (see AnyVal members, like ==)
          }
          tree.tpe = null
          super.typed(tree, mode, pt)
      }
    }
  }
}

