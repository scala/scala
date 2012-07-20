package scala.reflect.reify
package codegen

trait GenTypes {
  self: Reifier =>

  import global._
  import definitions._

  /**
   *  Reify a type.
   *  For internal use only, use ``reified'' instead.
   */
  def reifyType(tpe: Type): Tree = {
    assert(tpe != null, "tpe is null")

    if (tpe.isErroneous)
      CannotReifyErroneousReifee(tpe)
    if (tpe.isLocalToReifee)
      CannotReifyType(tpe)

    // this is a very special case. see the comments below for more info.
    if (isSemiConcreteTypeMember(tpe))
      return reifySemiConcreteTypeMember(tpe)

    // [Eugene] how do I check that the substitution is legal w.r.t tpe.info?
    val spliced = spliceType(tpe)
    if (spliced != EmptyTree)
      return spliced

    val tsym = tpe.typeSymbolDirect
    if (tsym.isClass && tpe == tsym.typeConstructor && tsym.isStatic)
      Select(Select(reify(tsym), nme.asTypeSymbol), nme.asTypeConstructor)
    else tpe match {
      case tpe @ NoType =>
        reifyMirrorObject(tpe)
      case tpe @ NoPrefix =>
        reifyMirrorObject(tpe)
      case tpe @ ThisType(root) if root.isRoot =>
        mirrorBuildCall(nme.thisPrefix, mirrorMirrorSelect(nme.RootClass))
      case tpe @ ThisType(empty) if empty.isEmptyPackageClass =>
        mirrorBuildCall(nme.thisPrefix, mirrorMirrorSelect(nme.EmptyPackageClass))
      case tpe @ ThisType(clazz) if clazz.isModuleClass && clazz.isStatic =>
        val module = reify(clazz.sourceModule)
        val moduleClass = Select(Select(module, nme.asModuleSymbol), nme.moduleClass)
        mirrorFactoryCall(nme.ThisType, moduleClass)
      case tpe @ ThisType(_) =>
        reifyProduct(tpe)
      case tpe @ SuperType(thistpe, supertpe) =>
        reifyProduct(tpe)
      case tpe @ SingleType(pre, sym) =>
        reifyProduct(tpe)
      case tpe @ ConstantType(value) =>
        mirrorFactoryCall(nme.ConstantType, reifyProduct(value))
      case tpe @ TypeRef(pre, sym, args) =>
        reifyProduct(tpe)
      case tpe @ TypeBounds(lo, hi) =>
        reifyProduct(tpe)
      case tpe @ NullaryMethodType(restpe) =>
        reifyProduct(tpe)
      case tpe @ AnnotatedType(anns, underlying, selfsym) =>
        reifyAnnotatedType(tpe)
      case _ =>
        reifyToughType(tpe)
    }
  }

  /** Keeps track of whether this reification contains abstract type parameters */
  def reificationIsConcrete: Boolean = state.reificationIsConcrete

  def spliceType(tpe: Type): Tree = {
    // [Eugene] it seems that depending on the context the very same symbol can be either a spliceable tparam or a quantified existential. very weird!
    val quantified = currentQuantified
    if (tpe.isSpliceable && !(quantified contains tpe.typeSymbol)) {
      if (reifyDebug) println("splicing " + tpe)

      val tagFlavor = if (concrete) tpnme.TypeTag.toString else tpnme.AbsTypeTag.toString
      val key = (tagFlavor, tpe.typeSymbol)
      // if this fails, it might produce the dreaded "erroneous or inaccessible type" error
      // to find out the whereabouts of the error run scalac with -Ydebug
      if (reifyDebug) println("launching implicit search for %s.%s[%s]".format(universe, tagFlavor, tpe))
      val result =
        typer.resolveTypeTag(defaultErrorPosition, universe.tpe, tpe, concrete = concrete, allowMaterialization = false) match {
          case failure if failure.isEmpty =>
            if (reifyDebug) println("implicit search was fruitless")
            if (reifyDebug) println("trying to splice as manifest")
            val splicedAsManifest = spliceAsManifest(tpe)
            if (splicedAsManifest.isEmpty) {
              if (reifyDebug) println("no manifest in scope")
              EmptyTree
            } else {
              if (reifyDebug) println("successfully spliced as manifest: " + splicedAsManifest)
              splicedAsManifest
            }
          case success =>
            if (reifyDebug) println("implicit search has produced a result: " + success)
            state.reificationIsConcrete &= concrete || success.tpe <:< TypeTagClass.asTypeConstructor
            Select(Apply(Select(success, nme.in), List(Ident(nme.MIRROR_SHORT))), nme.tpe)
        }
      if (result != EmptyTree) return result
      state.reificationIsConcrete = false
    }

    EmptyTree
  }

  private def spliceAsManifest(tpe: Type): Tree = {
    def isSynthetic(manifest: Tree) = manifest exists (sub => sub.symbol != null && (sub.symbol == FullManifestModule || sub.symbol.owner == FullManifestModule))
    def searchForManifest(typer: analyzer.Typer): Tree =
      analyzer.inferImplicit(
        EmptyTree,
        appliedType(FullManifestClass.asTypeConstructor, List(tpe)),
        reportAmbiguous = false,
        isView = false,
        context = typer.context,
        saveAmbiguousDivergent = false,
        pos = defaultErrorPosition) match {
          case success if !success.tree.isEmpty && !isSynthetic(success.tree) =>
            val manifestInScope = success.tree
            // todo. write a test for this
            if (ReflectRuntimeUniverse == NoSymbol) CannotConvertManifestToTagWithoutScalaReflect(tpe, manifestInScope)
            val cm = typer.typed(Ident(ReflectRuntimeCurrentMirror))
            val tagTree = gen.mkMethodCall(ReflectRuntimeUniverse, nme.manifestToTypeTag, List(tpe), List(cm, manifestInScope))
            Select(Apply(Select(tagTree, nme.in), List(Ident(nme.MIRROR_SHORT))), nme.tpe)
          case _ =>
            EmptyTree
        }
    val result = typer.silent(silentTyper => silentTyper.context.withMacrosDisabled(searchForManifest(silentTyper)))
    result match {
      case analyzer.SilentResultValue(result) => result
      case analyzer.SilentTypeError(_) => EmptyTree
    }
  }

  /** Reify a semi-concrete type member.
   *
   *  This is a VERY special case to deal with stuff like `typeOf[ru.Type]`.
   *  In that case `Type`, which is an abstract type member of scala.reflect.api.Universe, is not a free type.
   *  Why? Because we know its prefix, and it unambiguously determines the type.
   *
   *  Here is a different view on this question that supports this suggestion.
   *  Say, you reify a tree. Iff it doesn't contain free types, it can be successfully compiled and run.
   *  For example, if you reify `tpe.asInstanceOf[T]` taken from `def foo[T]`, then you won't be able to compile the result.
   *  Fair enough, you don't know the `T`, so the compiler will choke.
   *  This fact is captured by reification result having a free type T (this can be inspected by calling `tree.freeTypes`).
   *  Now imagine you reify the following tree: `tpe.asInstanceOf[ru.Type]`.
   *  To the contrast with the previous example, that's totally not a problem.
   *
   *  Okay, so we figured out that `ru.Type` is not a free type.
   *  However, in our reification framework, this type would be treated a free type.
   *  Why? Because `tpe.isSpliceable` will return true.
   *  Hence we intervene and handle this situation in a special way.
   *
   *  By the way, we cannot change the definition of `isSpliceable`, because class tags also depend on it.
   *  And, you know, class tags don't care whether we select a type member from a concrete instance or get it from scope (as with type parameters).
   *  The type itself still remains not concrete, in the sense that we don't know its erasure.
   *  I.e. we can compile the code that involves `ru.Type`, but we cannot serialize an instance of `ru.Type`.
   */
  private def reifySemiConcreteTypeMember(tpe: Type): Tree = tpe match {
    case tpe @ TypeRef(pre @ SingleType(prepre, presym), sym, args) if sym.isAbstractType && !sym.isExistential =>
      return mirrorFactoryCall(nme.TypeRef, reify(pre), mirrorBuildCall(nme.selectType, reify(sym.owner), reify(sym.name.toString)), reify(args))
  }

  /** Reify an annotated type, i.e. the one that makes us deal with AnnotationInfos */
  private def reifyAnnotatedType(tpe: AnnotatedType): Tree = {
    val AnnotatedType(anns, underlying, selfsym) = tpe
    mirrorFactoryCall(nme.AnnotatedType, mkList(anns map reifyAnnotationInfo), reify(underlying), reify(selfsym))
  }

  /** Reify a tough type, i.e. the one that leads to creation of auxiliary symbols */
  private def reifyToughType(tpe: Type): Tree = {
    if (reifyDebug) println("tough type: %s (%s)".format(tpe, tpe.kind))

    def reifyScope(scope: Scope): Tree = {
      scope foreach reifySymDef
      mirrorCall(nme.newScopeWith, scope.toList map reify: _*)
    }

    tpe match {
      case tpe @ RefinedType(parents, decls) =>
        reifySymDef(tpe.typeSymbol)
        mirrorFactoryCall(tpe, reify(parents), reifyScope(decls), reify(tpe.typeSymbol))
      case tpe @ ExistentialType(tparams, underlying) =>
        tparams foreach reifySymDef
        mirrorFactoryCall(tpe, reify(tparams), reify(underlying))
      case tpe @ ClassInfoType(parents, decls, clazz) =>
        reifySymDef(clazz)
        mirrorFactoryCall(tpe, reify(parents), reifyScope(decls), reify(tpe.typeSymbol))
      case tpe @ MethodType(params, restpe) =>
        params foreach reifySymDef
        mirrorFactoryCall(tpe, reify(params), reify(restpe))
      case tpe @ PolyType(tparams, underlying) =>
        tparams foreach reifySymDef
        mirrorFactoryCall(tpe, reify(tparams), reify(underlying))
      case _ =>
        throw new Error("internal error: %s (%s) is not supported".format(tpe, tpe.kind))
    }
  }
}
