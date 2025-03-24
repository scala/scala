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

package scala.tools.nsc
package typechecker

import scala.annotation._
import scala.collection.mutable
import scala.tools.nsc.symtab.Flags

/** Duplicate trees and re-type check them, taking care to replace
 *  and create fresh symbols for new local definitions.
 *
 *  @author  Iulian Dragos
 */
abstract class Duplicators extends Analyzer {
  import global._
  import definitions._

  /** Retype the given tree in the given context. Use this method when retyping
   *  a method in a different class. The typer will replace references to the this of
   *  the old class with the new class, and map symbols through the given 'env'. The
   *  environment is a map from type skolems to concrete types (see SpecializedTypes).
   */
  def retyped(context: Context, tree: Tree, oldThis: Symbol, newThis: Symbol, env: scala.collection.Map[Symbol, Type]): Tree = {
    if (oldThis ne newThis) {
      oldClassOwner = oldThis
      newClassOwner = newThis
    } else resetClassOwners()

    envSubstitution = new SubstSkolemsTypeMap(env.keysIterator.toList, env.valuesIterator.toList)
    debuglog("retyped with env: " + env)

    newBodyDuplicator(context).typed(tree)
  }

  protected def newBodyDuplicator(context: Context) = new BodyDuplicator(context)

  /** Return the special typer for duplicate method bodies. */
  override def newTyper(context: Context): Typer =
    newBodyDuplicator(context)

  private def resetClassOwners(): Unit = {
    oldClassOwner = null
    newClassOwner = null
  }

  private var oldClassOwner: Symbol = _
  private var newClassOwner: Symbol = _
  private var envSubstitution: SubstTypeMap = _

  private class SubstSkolemsTypeMap(from: List[Symbol], to: List[Type]) extends SubstTypeMap(from, to) {
    override protected def matches(sym1: Symbol, sym2: Symbol) =
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
          val sym1 = (
            context.scope lookup sym.name orElse {
              // try harder (look in outer scopes)
              // with virtpatmat, this could happen when the sym was referenced in the scope of a LabelDef but
              // was defined in the scope of an outer DefDef (e.g., in AbstractPartialFunction's andThen)
              BodyDuplicator.super.silent(_ typedType Ident(sym.name)).fold(NoSymbol: Symbol)(_.symbol)
            } filter (_ ne sym)
          )
          if (sym1.exists) {
            debuglog(s"fixing $sym -> $sym1")
            typeRef(NoPrefix, sym1, args mapConserve this)
          }
          else super.mapOver(tpe)

        case TypeRef(pre, sym, args) =>
          val newsym = updateSym(sym)
          if (newsym ne sym) {
            debuglog("fixing " + sym + " -> " + newsym)
            typeRef(mapOver(pre), newsym, args mapConserve this)
          } else
            super.mapOver(tpe)

        case SingleType(pre, sym) =>
          val sym1 = updateSym(sym)
          if (sym1 ne sym) {
            debuglog("fixing " + sym + " -> " + sym1)
            singleType(mapOver(pre), sym1)
          } else
            super.mapOver(tpe)

        case ThisType(sym) =>
          val sym1 = updateSym(sym)
          if (sym1 ne sym) {
            debuglog("fixing " + sym + " -> " + sym1)
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

    private def invalidate(tree: Tree, owner: Symbol = NoSymbol): Unit = {
      debuglog(s"attempting to invalidate symbol = ${tree.symbol}")
      if ((tree.isDef || tree.isInstanceOf[Function]) && tree.symbol != NoSymbol) {
        debuglog("invalid " + tree.symbol)
        invalidSyms(tree.symbol) = tree

        tree match {
          case ldef @ LabelDef(name, params, rhs) =>
            debuglog("LabelDef " + name + " sym.info: " + ldef.symbol.info)
            invalidSyms(ldef.symbol) = ldef
          //          breakIf(true, this, ldef, context)
            val newsym = ldef.symbol.cloneSymbol(context.owner)
            newsym.setInfo(fixType(ldef.symbol.info))
            ldef.symbol = newsym
            debuglog("newsym: " + newsym + " info: " + newsym.info)

          // don't retypecheck val members or local lazy vals -- you'll end up with duplicate symbols because
          // entering a valdef results in synthesizing getters etc
          // TODO: why retype check any valdefs?? I checked and the rhs is specialized just fine this way
          // (and there are no args/type params/... to warrant full type checking?)
          case vdef @ ValDef(mods, name, _, rhs) if mods.hasFlag(Flags.LAZY) || owner.isClass =>
            debuglog(s"ValDef $name in $owner sym.info: ${vdef.symbol.info}")
            invalidSyms(vdef.symbol) = vdef
            val newowner = owner orElse context.owner
            val newsym = vdef.symbol.cloneSymbol(newowner)
            newsym.setInfo(fixType(vdef.symbol.info))
            vdef.symbol = newsym
            debuglog("newsym: " + newsym + " info: " + newsym.info + ", owner: " + newsym.owner + ", " + newsym.owner.isClass)
            if (newsym.owner.isClass) newsym.owner.info.decls enter newsym

          case DefDef(_, name, tparams, vparamss, _, rhs) =>
            // invalidate parameters
            invalidateAll(tparams)
            vparamss foreach (x => invalidateAll(x))
            tree.symbol = NoSymbol

          case Function(vparams, _) =>
            // invalidate parameters
            invalidateAll(vparams)
            tree.symbol = NoSymbol

          case _ =>
            tree.symbol = NoSymbol
        }
      }
    }

    private def invalidateAll(stats: List[Tree], owner: Symbol = NoSymbol): Unit = {
      stats.foreach(invalidate(_, owner))
    }

    /** Optionally cast this tree into some other type, if required.
     *  Unless overridden, just returns the tree.
     */
    def castType(tree: Tree, @unused pt: Type): Tree = tree

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
    override def typed(tree: Tree, mode: Mode, pt: Type): Tree = {
      debuglog("typing " + tree + ": " + tree.tpe + ", " + tree.getClass)
      val origtreesym = tree.symbol
      if (tree.hasSymbolField && tree.symbol != NoSymbol
          && !tree.symbol.isLabel  // labels cannot be retyped by the type checker as LabelDef has no ValDef/return type trees
          && invalidSyms.isDefinedAt(tree.symbol)) {
        debuglog("removed symbol " + tree.symbol)
        tree.symbol = NoSymbol
      }

      tree match {
        case ttree @ TypeTree() =>
          // log("fixing tpe: " + tree.tpe + " with sym: " + tree.tpe.typeSymbol)
          ttree modifyType fixType

        case Block(stats, res) =>
          debuglog("invalidating block")
          invalidateAll(stats)
          invalidate(res)
          super.typed(tree.clearType(), mode, pt)

        case ClassDef(_, _, _, tmpl @ Template(parents, _, stats)) =>
          // log("invalidating classdef " + tree)
          tmpl.symbol = tree.symbol.newLocalDummy(tree.pos)
          invalidateAll(stats, tree.symbol)
          super.typed(tree.clearType(), mode, pt)

        case ddef @ DefDef(_, _, _, _, tpt, rhs) =>
          ddef.tpt modifyType fixType
          val result = super.typed(ddef.clearType(), mode, pt)
          // TODO this is a hack, we really need a cleaner way to transport symbol attachments to duplicated methods
          // bodies in specialized subclasses.
          if (ddef.hasAttachment[DelambdafyTarget.type])
            result.symbol.updateAttachment(DelambdafyTarget)
          result

        case vdef @ ValDef(mods, name, tpt, rhs) =>
          // log("vdef fixing tpe: " + tree.tpe + " with sym: " + tree.tpe.typeSymbol + " and " + invalidSyms)
          //if (mods.hasFlag(Flags.LAZY)) vdef.symbol.resetFlag(Flags.MUTABLE) // Martin to Iulian: lazy vars can now appear because they are no longer boxed; Please check that deleting this statement is OK.
          vdef.tpt modifyType fixType
          super.typed(vdef.clearType(), mode, pt)

        case ldef @ LabelDef(name, params, rhs) =>
          // log("label def: " + ldef)
          // in case the rhs contains any definitions -- TODO: is this necessary?
          invalidate(rhs)
          ldef.clearType()

          // is this LabelDef generated by tailcalls?
          val isTailLabel = (ldef.params.lengthIs >= 1) && (ldef.params.head.name == nme.THIS)

          // the typer does not create the symbols for a LabelDef's params, so unless they were created before we need
          // to do it manually here -- but for the tailcalls-generated labels, ValDefs are created before the LabelDef,
          // so we just need to change the tree to point to the updated symbols
          def newParam(p: Tree): Ident =
            if (isTailLabel)
              Ident(updateSym(p.symbol))
            else {
              val newsym = p.symbol.cloneSymbol //(context.owner) // TODO owner?
              Ident(newsym.setInfo(fixType(p.symbol.info)))
            }

          val params1 = params map newParam
          val rhs1 = (new TreeSubstituter(params map (_.symbol), params1) transform rhs) // TODO: duplicate?

          super.typed(treeCopy.LabelDef(tree, name, params1, rhs1.clearType()), mode, pt)

        case Bind(name, _) =>
          // log("bind: " + tree)
          invalidate(tree)
          super.typed(tree.clearType(), mode, pt)

        case Ident(_) if tree.symbol.isLabel =>
          debuglog("Ident to labeldef " + tree + " switched to ")
          tree.symbol = updateSym(tree.symbol)
          super.typed(tree.clearType(), mode, pt)

        case Ident(_) if (origtreesym ne null) && origtreesym.isLazy =>
          debuglog("Ident to a lazy val " + tree + ", " + tree.symbol + " updated to " + origtreesym)
          tree.symbol = updateSym(origtreesym)
          super.typed(tree.clearType(), mode, pt)

        case Select(th @ This(_), _) if (oldClassOwner ne null) && (th.symbol == oldClassOwner) =>
          // We use the symbol name instead of the tree name because the symbol
          // may have been name mangled, rendering the tree name obsolete.
          // ...but you can't just do a Select on a name because if the symbol is
          // overloaded, you will crash in the backend.
          val memberByName  = newClassOwner.thisType.member(tree.symbol.name)
          def nameSelection = Select(This(newClassOwner), tree.symbol.name)
          val newTree = (
            if (memberByName.isOverloaded) {
              // Find the types of the overload alternatives as seen in the new class,
              // and filter the list down to those which match the old type (after
              // fixing the old type so it is seen as if from the new class.)
              val typeInNewClass = fixType(oldClassOwner.info memberType tree.symbol)
              val alts           = memberByName.alternatives
              val memberTypes    = alts map (newClassOwner.info memberType _)
              val memberString   = memberByName.defString
              alts zip memberTypes filter (_._2 =:= typeInNewClass) match {
                case ((alt, tpe)) :: Nil =>
                  log(s"Arrested overloaded type in Duplicators, narrowing to ${alt.defStringSeenAs(tpe)}\n  Overload was: $memberString")
                  Select(This(newClassOwner), alt)
                case _ =>
                  alts filter (alt => (alt.paramss corresponds tree.symbol.paramss)(_.size == _.size)) match {
                    case alt :: Nil =>
                      log(s"Resorted to parameter list arity to disambiguate to $alt\n  Overload was: $memberString")
                      Select(This(newClassOwner), alt)
                    case _ =>
                      log(s"Could not disambiguate $memberTypes. Attempting name-based selection, but we may crash later.")
                      nameSelection
                  }
              }
            }
            else nameSelection
          )
          super.typed(atPos(tree.pos)(newTree), mode, pt)

        case This(_) if (oldClassOwner ne null) && (tree.symbol == oldClassOwner) =>
//          val tree1 = Typed(This(newClassOwner), TypeTree(fixType(tree.tpe.widen)))
          // log("selection on this: " + tree)
          val tree1 = This(newClassOwner)
          // log("tree1: " + tree1)
          debuglog("mapped " + tree + " to " + tree1)
          super.typedPos(tree.pos, mode, pt)(tree1)

        case This(_) =>
          debuglog("selection on this, plain: " + tree)
          tree.symbol = updateSym(tree.symbol)
          val ntree = castType(tree, pt)
          val tree1 = super.typed(ntree, mode, pt)
          // log("plain this typed to: " + tree1)
          tree1
/* no longer needed, because Super now contains a This(...)
        case Super(qual, mix) if (oldClassOwner ne null) && (tree.symbol == oldClassOwner) =>
          val tree1 = Super(qual, mix)
          log("changed " + tree + " to " + tree1)
          super.typed(atPos(tree.pos)(tree1))
*/
        case Match(scrut, cases) =>
          val scrut1   = typedByValueExpr(scrut)
          val scrutTpe = scrut1.tpe.widen
          val cases1 = {
            if (scrutTpe.isFinalType) cases filter {
              case CaseDef(Bind(_, Typed(_, tpt)), EmptyTree, body) =>
                // the typed pattern is not incompatible with the scrutinee type
                scrutTpe matchesPattern fixType(tpt.tpe)
              case CaseDef(Typed(_, tpt), EmptyTree, body) =>
                // the typed pattern is not incompatible with the scrutinee type
                scrutTpe matchesPattern fixType(tpt.tpe)
              case _ => true
            }
            // Without this, AnyRef specializations crash on patterns like
            //   case _: Boolean => ...
            // Not at all sure this is safe.
            else if (scrutTpe <:< AnyRefTpe)
              cases filterNot (_.pat.tpe <:< AnyValTpe)
            else
              cases
          }

          super.typed(atPos(tree.pos)(Match(scrut, cases1)), mode, pt)

        case EmptyTree =>
          // no need to do anything, in particular, don't set the type to null, EmptyTree.tpe_= asserts
          tree

        case _ =>
          debuglog("Duplicators default case: " + tree.summaryString)
          debuglog(" ---> " + tree)
          if (tree.hasSymbolField && tree.symbol.safeOwner == AnyClass)
            tree.symbol = NoSymbol // maybe we can find a more specific member in a subclass of Any (see AnyVal members, like ==)

          val ntree = castType(tree, pt)
          super.typed(ntree, mode, pt)
      }
    }

  }
}

