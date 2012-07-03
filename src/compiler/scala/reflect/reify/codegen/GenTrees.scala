package scala.reflect.reify
package codegen

trait GenTrees {
  self: Reifier =>

  import global._
  import definitions._

  // unfortunately, these are necessary to reify AnnotatedTypes
  // I'd gladly get rid of them, but I don't fancy making a metaprogramming API that doesn't work with annotated types
  // luckily for our sanity, these vars are mutated only within a very restricted code execution path
  def reifyTreeSymbols: Boolean = state.reifyTreeSymbols
  def reifyTreeTypes: Boolean = state.reifyTreeTypes

  /**
   *  Reify a tree.
   *  For internal use only, use ``reified'' instead.
   */
  def reifyTree(tree: Tree): Tree = {
    assert(tree != null, "tree is null")

    if (tree.isErroneous)
      CannotReifyErroneousReifee(tree)

    val splicedTree = spliceTree(tree)
    if (splicedTree != EmptyTree)
      return splicedTree

    // the idea behind the new reincarnation of reifier is a simple maxim:
    //
    //   never call ``reifyType'' to reify a tree
    //
    // this works because the stuff we are reifying was once represented with trees only
    // and lexical scope information can be fully captured by reifying symbols
    //
    // to enable this idyll, we work hard in the ``Reshape'' phase
    // which replaces all types with equivalent trees and works around non-idempotencies of the typechecker
    //
    // why bother? because this brings method to the madness
    // the first prototype of reification reified all types and symbols for all trees => this quickly became unyieldy
    // the second prototype reified external types, but avoided reifying local ones => this created an ugly irregularity
    // current approach is uniform and compact
    var rtree = tree match {
      case global.EmptyTree =>
        reifyMirrorObject(EmptyTree)
      case global.emptyValDef =>
        mirrorBuildSelect(nme.emptyValDef)
      case FreeDef(_, _, _, _, _) =>
        reifyNestedFreeDef(tree)
      case FreeRef(_, _) =>
        reifyNestedFreeRef(tree)
      case BoundTerm(tree) =>
        reifyBoundTerm(tree)
      case BoundType(tree) =>
        reifyBoundType(tree)
      case Literal(const @ Constant(_)) =>
        mirrorCall(nme.Literal, reifyProduct(const))
      case Import(expr, selectors) =>
        mirrorCall(nme.Import, reify(expr), mkList(selectors map reifyProduct))
      case _ =>
        reifyProduct(tree)
    }

    // usually we don't reify symbols/types, because they can be re-inferred during subsequent reflective compilation
    // however, reification of AnnotatedTypes is special. see ``reifyType'' to find out why.
    if (reifyTreeSymbols && tree.hasSymbol) {
      if (reifyDebug) println("reifying symbol %s for tree %s".format(tree.symbol, tree))
      rtree = mirrorBuildCall(nme.setSymbol, rtree, reify(tree.symbol))
    }
    if (reifyTreeTypes && tree.tpe != null) {
      if (reifyDebug) println("reifying type %s for tree %s".format(tree.tpe, tree))
      rtree = mirrorBuildCall(nme.setType, rtree, reify(tree.tpe))
    }

    rtree
  }

  def reifyModifiers(m: global.Modifiers) =
    mirrorFactoryCall(nme.Modifiers, mirrorBuildCall(nme.flagsFromBits, reify(m.flags)), reify(m.privateWithin), reify(m.annotations))

  private def spliceTree(tree: Tree): Tree = {
    tree match {
      case TreeSplice(splicee) =>
        if (reifyDebug) println("splicing " + tree)

        // see ``Metalevels'' for more info about metalevel breaches
        // and about how we deal with splices that contain them
        val isMetalevelBreach = splicee exists (sub => sub.hasSymbol && sub.symbol != NoSymbol && sub.symbol.metalevel > 0)
        val isRuntimeEval = splicee exists (sub => sub.hasSymbol && sub.symbol == ExprSplice)
        if (isMetalevelBreach || isRuntimeEval) {
          // we used to convert dynamic splices into runtime evals transparently, but we no longer do that
          // why? see comments in ``Metalevels''
          // if (reifyDebug) println("splicing has failed: cannot splice when facing a metalevel breach")
          // EmptyTree
          CannotReifyRuntimeSplice(tree)
        } else {
          if (reifyDebug) println("splicing has succeeded")
          splicee match {
            // we intentionally don't care about the prefix (the first underscore in the `RefiedTree` pattern match)
            case ReifiedTree(_, _, inlinedSymtab, rtree, _, _, _) =>
              if (reifyDebug) println("inlining the splicee")
              // all free vars local to the enclosing reifee should've already been inlined by ``Metalevels''
              inlinedSymtab.syms foreach (sym => if (sym.isLocalToReifee) assert(false, inlinedSymtab.symDef(sym)))
              state.symtab ++= inlinedSymtab
              rtree
            case tree =>
              val migrated = Apply(Select(splicee, nme.in), List(Ident(nme.MIRROR_SHORT)))
              Select(migrated, nme.tree)
          }
        }
      case _ =>
        EmptyTree
    }
  }

  // unlike in `reifyBoundType` we can skip checking for `tpe` being local or not local w.r.t the reifee
  // a single check for a symbol of the bound term should be enough
  // that's because only Idents and Thises can be bound terms, and they cannot host complex types
  private def reifyBoundTerm(tree: Tree): Tree = tree match {
    case tree @ This(_) if tree.symbol == NoSymbol =>
      throw new Error("unexpected: bound term that doesn't have a symbol: " + showRaw(tree))
    case tree @ This(_) if tree.symbol.isClass && !tree.symbol.isModuleClass && !tree.symbol.isLocalToReifee =>
      val sym = tree.symbol
      if (reifyDebug) println("This for %s, reified as freeVar".format(sym))
      if (reifyDebug) println("Free: " + sym)
      mirrorBuildCall(nme.Ident, reifyFreeTerm(sym, This(sym)))
    case tree @ This(_) if !tree.symbol.isLocalToReifee =>
      if (reifyDebug) println("This for %s, reified as This".format(tree.symbol))
      mirrorBuildCall(nme.This, reify(tree.symbol))
    case tree @ This(_) if tree.symbol.isLocalToReifee =>
      mirrorCall(nme.This, reify(tree.qual))
    case tree @ Ident(_) if tree.symbol == NoSymbol =>
      // this sometimes happens, e.g. for binds that don't have a body
      // or for untyped code generated during previous phases
      // (see a comment in Reifiers about the latter, starting with "why do we resetAllAttrs?")
      mirrorCall(nme.Ident, reify(tree.name))
    case tree @ Ident(_) if !tree.symbol.isLocalToReifee =>
      if (tree.symbol.isVariable && tree.symbol.owner.isTerm) {
        captureVariable(tree.symbol) // Note order dependency: captureVariable needs to come before reification here.
        mirrorCall(nme.Select, mirrorBuildCall(nme.Ident, reify(tree.symbol)), reify(nme.elem))
      } else {
        mirrorBuildCall(nme.Ident, reify(tree.symbol))
      }
    case tree @ Ident(_) if tree.symbol.isLocalToReifee =>
      mirrorCall(nme.Ident, reify(tree.name))
    case _ =>
      throw new Error("internal error: %s (%s, %s) is not supported".format(tree, tree.productPrefix, tree.getClass))
  }

  private def reifyBoundType(tree: Tree): Tree = {
    def reifyBoundType(tree: Tree): Tree = {
      if (tree.tpe == null)
        throw new Error("unexpected: bound type that doesn't have a tpe: " + showRaw(tree))

      // if a symbol or a type of the scrutinee are local to reifee
      // (e.g. point to a locally declared class or to a path-dependent thingie that depends on a local variable)
      // then we can reify the scrutinee as a symless AST and that will definitely be hygienic
      // why? because then typechecking of a scrutinee doesn't depend on the environment external to the quasiquote
      // otherwise we need to reify the corresponding type
      if (tree.symbol.isLocalToReifee || tree.tpe.isLocalToReifee)
        reifyProduct(tree)
      else {
        val sym = tree.symbol
        val tpe = tree.tpe
        if (reifyDebug) println("reifying bound type %s (underlying type is %s)".format(sym, tpe))

        if (tpe.isSpliceable) {
          val spliced = spliceType(tpe)
          if (spliced == EmptyTree) {
            if (reifyDebug) println("splicing failed: reify as is")
            mirrorBuildCall(nme.TypeTree, reify(tpe))
          } else {
            spliced match {
              case TypeRefToFreeType(freeType) =>
                if (reifyDebug) println("splicing returned a free type: " + freeType)
                Ident(freeType)
              case _ =>
                if (reifyDebug) println("splicing succeeded: " + spliced)
                mirrorBuildCall(nme.TypeTree, spliced)
            }
          }
        } else {
          if (sym.isLocatable) {
            if (reifyDebug) println("tpe is locatable: reify as Ident(%s)".format(sym))
            mirrorBuildCall(nme.Ident, reify(sym))
          } else {
            if (reifyDebug) println("tpe is not locatable: reify as TypeTree(%s)".format(tpe))
            mirrorBuildCall(nme.TypeTree, reify(tpe))
          }
        }
      }
    }

    tree match {
      case Select(_, _) =>
        reifyBoundType(tree)
      case SelectFromTypeTree(_, _) =>
        reifyBoundType(tree)
      case Ident(_) =>
        reifyBoundType(tree)
      case _ =>
        throw new Error("internal error: %s (%s, %s) is not supported".format(tree, tree.productPrefix, tree.getClass))
    }
  }

  private def reifyNestedFreeDef(tree: Tree): Tree = {
    if (reifyDebug) println("nested free def: %s".format(showRaw(tree)))
    reifyProduct(tree)
  }

  private def reifyNestedFreeRef(tree: Tree): Tree = {
    if (reifyDebug) println("nested free ref: %s".format(showRaw(tree)))
    reifyProduct(tree)
  }
}
