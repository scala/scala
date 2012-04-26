package scala.reflect.reify
package codegen

trait Trees {
  self: Reifier =>

  import mirror._
  import definitions._
  import treeInfo._
  import scala.reflect.api.Modifier

  // unfortunately, these are necessary to reify AnnotatedTypes
  // I'd gladly got rid of them, but I don't fancy making a metaprogramming API that doesn't work with annotated types
  var reifyTreeSymbols = false
  var reifyTreeTypes = false

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
      case mirror.EmptyTree =>
        reifyMirrorObject(EmptyTree)
      case mirror.emptyValDef =>
        mirrorSelect(nme.emptyValDef)
      case FreeDef(_, _, _, _, _) =>
        reifyNestedFreeDef(tree)
      case FreeRef(_, _) =>
        reifyNestedFreeRef(tree)
      case BoundTerm(tree) =>
        reifyBoundTerm(tree)
      case BoundType(tree) =>
        reifyBoundType(tree)
      case NestedExpr(_, _, _) =>
        reifyNestedExpr(tree)
      case Literal(const @ Constant(_)) =>
        // [Eugene] was necessary when we reified erasures as normalized tycons
        // now, when we do existentialAbstraction on normalizations, everything works great
        // todo. find an explanation
//        if (const.tag == ClazzTag) {
////          def preprocess(tpe: Type): Type = tpe.typeSymbol match {
////            case ArrayClass => appliedType(ArrayClass, preprocess(tpe.typeArgs.head))
////            case _ => tpe.typeConstructor
////          }
////          val tpe = preprocess(const.typeValue)
//          val tpe = const.typeValue
//          var reified = reify(tpe)
//          reified = mirrorCall(nme.Literal, mirrorCall(nme.Constant, reified))
////          val skolems = ClassClass.typeParams map (_ => newTypeName(typer.context.unit.fresh.newName("_$")))
////          var existential = mirrorCall(nme.AppliedTypeTree, mirrorCall(nme.TypeTree, reify(ClassClass.typeConstructor)), mkList(skolems map (skolem => mirrorCall(nme.Ident, reify(skolem)))))
////          existential = mirrorCall(nme.ExistentialTypeTree, existential, reify(skolems map (skolem => TypeDef(Modifiers(Set(Modifier.deferred: Modifier)), skolem, Nil, TypeBoundsTree(Ident(NothingClass) setType NothingClass.tpe, Ident(AnyClass) setType AnyClass.tpe)))))
////          reified = mirrorCall(nme.TypeApply, mirrorCall(nme.Select, reified, reify(nme.asInstanceOf_)), mkList(List(existential)))
//          // why is this required??
////          reified = mirrorCall(nme.TypeApply, mirrorCall(nme.Select, reified, reify(nme.asInstanceOf_)), mkList(List(mirrorCall(nme.TypeTree, reify(appliedType(ClassClass.tpe, List(AnyClass.tpe)))))))
//          reified
//        } else {
//          mirrorCall(nme.Literal, reifyProduct(const))
//        }
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
      rtree = Apply(Select(rtree, nme.setSymbol), List(reify(tree.symbol)))
    }
    if (reifyTreeTypes && tree.tpe != null) {
      if (reifyDebug) println("reifying type %s for tree %s".format(tree.tpe, tree))
      rtree = Apply(Select(rtree, nme.setType), List(reify(tree.tpe)))
    }

    rtree
  }

  def reifyModifiers(m: mirror.Modifiers) =
    mirrorCall("modifiersFromInternalFlags", reify(m.flags), reify(m.privateWithin), reify(m.annotations))

  private def spliceTree(tree: Tree): Tree = {
    tree match {
      case EvalSplice(splicee) =>
        if (reifyDebug) println("splicing eval " + tree)

        // see ``Metalevels'' for more info about metalevel breaches
        // and about how we deal with splices that contain them
        if (splicee exists (sub => sub.hasSymbol && sub.symbol != NoSymbol && sub.symbol.metalevel > 0)) {
          if (reifyDebug) println("splicing has failed: cannot splice when facing a metalevel breach")
          EmptyTree
        } else {
          if (reifyDebug) println("splicing has succeeded")
          var splice = Select(splicee, nme.tree)
          splice match {
            case InlinedTreeSplice(_, inlinedSymbolTable, tree, _) =>
              if (reifyDebug) println("inlining the splicee")
              // all free vars local to the enclosing reifee should've already been inlined by ``Metalevels''
              inlinedSymbolTable collect { case freedef @ FreeDef(_, _, binding, _, _) if binding.symbol.isLocalToReifee => assert(false, freedef) }
              symbolTable ++= inlinedSymbolTable
              tree
            case tree =>
              // we need to preserve types of exprs, because oftentimes they cannot be inferred later
              // this circumvents regular reification scheme, therefore we go the extra mile here
              new Transformer {
                override def transform(tree: Tree) = super.transform(tree match {
                  case NestedExpr(factory, tree, typetag) =>
                    val typedFactory = TypeApply(factory, List(TypeTree(typetag.tpe.typeArgs(0))))
                    Apply(Apply(typedFactory, List(tree)), List(typetag))
                  case _ =>
                    tree
                })
              }.transform(tree)
          }
        }
      case ValueSplice(splicee) =>
        // todo. implement this
        ???
      case _ =>
        EmptyTree
    }
  }

  private def reifyBoundTerm(tree: Tree): Tree = tree match {
    case tree @ This(_) if tree.symbol == NoSymbol =>
      throw new Error("unexpected: bound term that doesn't have a symbol: " + showRaw(tree))
    case tree @ This(_) if tree.symbol.isClass && !tree.symbol.isModuleClass && !tree.symbol.isLocalToReifee =>
      val sym = tree.symbol
      if (reifyDebug) println("This for %s, reified as freeVar".format(sym))
      if (reifyDebug) println("Free: " + sym)
      mirrorCall(nme.Ident, reifyFreeTerm(sym, This(sym)))
    case tree @ This(_) if !tree.symbol.isLocalToReifee =>
      if (reifyDebug) println("This for %s, reified as This".format(tree.symbol))
      mirrorCall(nme.This, reify(tree.symbol))
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
        mirrorCall(nme.Select, mirrorCall(nme.Ident, reify(tree.symbol)), reify(nme.elem))
      } else {
        mirrorCall(nme.Ident, reify(tree.symbol))
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

      if (tree.symbol.isLocalToReifee)
        reifyProduct(tree)
      else {
        val sym0 = tree.symbol
        val sym = sym0.dealias
        val tpe0 = tree.tpe
        val tpe = tpe0.dealias
        if (reifyDebug) println("reifying bound type %s (underlying type is %s, dealiased is %s)".format(sym0, tpe0, tpe))

        if (tpe.isSpliceable) {
          val spliced = spliceType(tpe)
          if (spliced == EmptyTree) {
            if (reifyDebug) println("splicing failed: reify as is")
            mirrorCall(nme.TypeTree, reify(tpe))
          } else {
            spliced match {
              case TypeRefToFreeType(freeType) =>
                if (reifyDebug) println("splicing returned a free type: " + freeType)
                Ident(freeType)
              case _ =>
                if (reifyDebug) println("splicing succeeded: " + spliced)
                mirrorCall(nme.TypeTree, spliced)
            }
          }
        } else {
          if (sym.isLocatable) {
            if (reifyDebug) println("tpe is locatable: reify as Ident(%s)".format(sym))
            mirrorCall(nme.Ident, reify(sym))
          } else {
            if (reifyDebug) println("tpe is an alias, but not a locatable: reify as TypeTree(%s)".format(tpe))
            mirrorCall(nme.TypeTree, reify(tpe))
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

  private def reifyNestedFreeRef(tree: Tree): Tree = tree match {
    case Apply(Select(mrRef @ Ident(_), ident), List(Ident(name: TermName))) if ident == nme.Ident && name.startsWith(nme.MIRROR_FREE_PREFIX) =>
      if (reifyDebug) println("nested free ref: %s".format(showRaw(tree)))
      reifyProduct(tree)
    case _ =>
      throw new Error("internal error: %s (%s, %s) is not supported".format(tree, tree.productPrefix, tree.getClass))
  }

  private def reifyNestedExpr(tree: Tree): Tree = tree match {
    case NestedExpr(factory, tree, typetag) =>
      // we need to preserve types of exprs, because oftentimes they cannot be inferred later
      // this circumvents regular reification scheme, therefore we go through this crazy dance
      if (reifyDebug) println("nested expr: %s".format(showRaw(tree)))
      val rtype = mirrorCall(nme.TypeTree, reify(typetag.tpe.typeArgs(0)))
      val rfactory = mirrorCall(nme.TypeApply, reify(factory), mkList(List(rtype)))
      val rexpr = mirrorCall(nme.Apply, rfactory, reify(List(tree)))
      val rwrapped = mirrorCall(nme.Apply, rexpr, reify(List(typetag)))
      rwrapped
    case _ =>
      throw new Error("internal error: %s (%s, %s) is not supported".format(tree, tree.productPrefix, tree.getClass))
  }
}