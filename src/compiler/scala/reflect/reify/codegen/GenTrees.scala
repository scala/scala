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
   *  For internal use only, use `reified` instead.
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
    //   never call `reifyType` to reify a tree
    //
    // this works because the stuff we are reifying was once represented with trees only
    // and lexical scope information can be fully captured by reifying symbols
    //
    // to enable this idyll, we work hard in the `Reshape` phase
    // which replaces all types with equivalent trees and works around non-idempotencies of the typechecker
    //
    // why bother? because this brings method to the madness
    // the first prototype of reification reified all types and symbols for all trees => this quickly became unyieldy
    // the second prototype reified external types, but avoided reifying ones local to the reifee => this created an ugly irregularity
    // current approach is uniform and compact
    var rtree: Tree = tree match {
      case FreeDef(_, _, _, _, _) => reifyNestedFreeDef(tree)
      case FreeRef(_, _)          => reifyNestedFreeRef(tree)
      case BoundTerm(tree)        => reifyBoundTerm(tree)
      case BoundType(tree)        => reifyBoundType(tree)
      case _                      => reifyTreeSyntactically(tree)
    }

    // usually we don't reify symbols/types, because they can be re-inferred during subsequent reflective compilation
    // however, reification of AnnotatedTypes is special. see `reifyType` to find out why.
    if (reifyTreeSymbols && tree.hasSymbolField) {
      if (reifyDebug) println("reifying symbol %s for tree %s".format(tree.symbol, tree))
      rtree = mirrorBuildCall(nme.setSymbol, rtree, reify(tree.symbol))
    }
    if (reifyTreeTypes && tree.tpe != null) {
      if (reifyDebug) println("reifying type %s for tree %s".format(tree.tpe, tree))
      rtree = mirrorBuildCall(nme.setType, rtree, reify(tree.tpe))
    }

    rtree
  }

  def reifyTreeSyntactically(tree: Tree): Tree = tree match {
    case global.EmptyTree             => reifyMirrorObject(EmptyTree)
    case global.noSelfType            => mirrorSelect(nme.noSelfType)
    case global.pendingSuperCall      => mirrorSelect(nme.pendingSuperCall)
    case Literal(const @ Constant(_)) => mirrorCall(nme.Literal, reifyProduct(const))
    case Import(expr, selectors)      => mirrorCall(nme.Import, reify(expr), mkList(selectors map reifyProduct))
    case _                            => reifyProduct(tree)
  }

  def reifyFlags(flags: FlagSet) =
    if (flags != 0) reifyBuildCall(nme.FlagsRepr, flags) else mirrorSelect(nme.NoFlags)

  def reifyModifiers(m: global.Modifiers) =
    if (m == NoMods) mirrorSelect(nme.NoMods)
    else mirrorFactoryCall(nme.Modifiers, reifyFlags(m.flags), reify(m.privateWithin), reify(m.annotations))

  private def spliceTree(tree: Tree): Tree = {
    tree match {
      case TreeSplice(splicee) =>
        if (reifyDebug) println("splicing " + tree)

        // see `Metalevels` for more info about metalevel breaches
        // and about how we deal with splices that contain them
        val isMetalevelBreach = splicee exists (sub => sub.hasSymbolField && sub.symbol != NoSymbol && sub.symbol.metalevel > 0)
        val isRuntimeEval = splicee exists (sub => sub.hasSymbolField && sub.symbol == ExprSplice)
        if (isMetalevelBreach || isRuntimeEval) {
          // we used to convert dynamic splices into runtime evals transparently, but we no longer do that
          // why? see comments in `Metalevels`
          // if (reifyDebug) println("splicing has failed: cannot splice when facing a metalevel breach")
          // EmptyTree
          CannotReifyRuntimeSplice(tree)
        } else {
          if (reifyDebug) println("splicing has succeeded")
          splicee match {
            // we intentionally don't care about the prefix (the first underscore in the `RefiedTree` pattern match)
            case ReifiedTree(_, _, inlinedSymtab, rtree, _, _, _) =>
              if (reifyDebug) println("inlining the splicee")
              // all free vars local to the enclosing reifee should've already been inlined by `Metalevels`
              for (sym <- inlinedSymtab.syms if sym.isLocalToReifee)
                abort("free var local to the reifee, should have already been inlined by Metalevels: " + inlinedSymtab.symDef(sym))
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
  private def reifyBoundTerm(tree: Tree): Tree = {
    val sym = tree.symbol

    tree match {
      case This(qual) =>
        assert(sym != NoSymbol, "unexpected: bound term that doesn't have a symbol: " + showRaw(tree))
        if (sym.isLocalToReifee)
          mirrorCall(nme.This, reify(qual))
        else if (sym.isClass && !sym.isModuleClass) {
          if (reifyDebug) println("This for %s, reified as freeVar".format(sym))
          if (reifyDebug) println("Free: " + sym)
          mirrorBuildCall(nme.mkIdent, reifyFreeTerm(This(sym)))
        }
        else {
          if (reifyDebug) println("This for %s, reified as This".format(sym))
          mirrorBuildCall(nme.mkThis, reify(sym))
        }

      case Ident(name) =>
        if (sym == NoSymbol) {
          // this sometimes happens, e.g. for binds that don't have a body
          // or for untyped code generated during previous phases
          // (see a comment in Reifiers about the latter, starting with "why do we reset attrs?")
          mirrorCall(nme.Ident, reify(name))
        }
        else if (!sym.isLocalToReifee) {
          if (sym.isVariable && sym.owner.isTerm) {
            captureVariable(sym) // Note order dependency: captureVariable needs to come before reification here.
            mirrorCall(nme.Select, mirrorBuildCall(nme.mkIdent, reify(sym)), reify(nme.elem))
          }
          else mirrorBuildCall(nme.mkIdent, reify(sym))
        }
        else mirrorCall(nme.Ident, reify(name))

      case Select(qual, name) =>
        if (qual.symbol != null && qual.symbol.hasPackageFlag) {
          mirrorBuildCall(nme.mkIdent, reify(sym))
        } else {
          val effectiveName = if (sym != null && sym != NoSymbol) sym.name else name
          reifyProduct(Select(qual, effectiveName))
        }

      case _ =>
        throw new Error("internal error: %s (%s, %s) is not supported".format(tree, tree.productPrefix, tree.getClass))
    }
  }

  private def reifyBoundType(tree: RefTree): Tree = {
    val sym = tree.symbol
    val tpe = tree.tpe

    def reifyBoundType(tree: RefTree): Tree = {
      assert(tpe != null, "unexpected: bound type that doesn't have a tpe: " + showRaw(tree))

      // if a symbol or a type of the scrutinee are local to reifee
      // (e.g. point to a locally declared class or to a path-dependent thingie that depends on a variable defined within the reifee)
      // then we can reify the scrutinee as a symless AST and that will definitely be hygienic
      // why? because then typechecking of a scrutinee doesn't depend on the environment external to the quasiquote
      // otherwise we need to reify the corresponding type
      if (sym.isLocalToReifee || tpe.isLocalToReifee || treeInfo.isWildcardStarType(tree))
        reifyProduct(tree)
      else {
        if (reifyDebug) println("reifying bound type %s (underlying type is %s)".format(sym, tpe))

        if (tpe.isSpliceable) {
          val spliced = spliceType(tpe)

          if (spliced == EmptyTree) {
            if (reifyDebug) println("splicing failed: reify as is")
            mirrorBuildCall(nme.mkTypeTree, reify(tpe))
          }
          else spliced match {
            case TypeRefToFreeType(freeType) =>
              if (reifyDebug) println("splicing returned a free type: " + freeType)
              Ident(freeType)
            case _ =>
              if (reifyDebug) println("splicing succeeded: " + spliced)
              mirrorBuildCall(nme.mkTypeTree, spliced)
          }
        }
        else tree match {
          case Select(qual, name) if !qual.symbol.hasPackageFlag =>
            if (reifyDebug) println(s"reifying Select($qual, $name)")
            mirrorCall(nme.Select, reify(qual), reify(name))
          case SelectFromTypeTree(qual, name) =>
            if (reifyDebug) println(s"reifying SelectFromTypeTree($qual, $name)")
            mirrorCall(nme.SelectFromTypeTree, reify(qual), reify(name))
          case _ if sym.isLocatable =>
            if (reifyDebug) println(s"tpe is locatable: reify as Ident($sym)")
            mirrorBuildCall(nme.mkIdent, reify(sym))
          case _ =>
            if (reifyDebug) println(s"tpe is not locatable: reify as TypeTree($tpe)")
            mirrorBuildCall(nme.mkTypeTree, reify(tpe))
        }
      }
    }

    tree match {
      case Select(qual, name) if name != sym.name =>
        reifyBoundType(Select(qual, sym.name))

      case Select(_, _) | SelectFromTypeTree(_, _) | Ident(_) =>
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
