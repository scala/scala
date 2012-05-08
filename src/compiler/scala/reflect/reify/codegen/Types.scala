package scala.reflect.reify
package codegen

trait Types {
  self: Reifier =>

  import mirror._
  import definitions._
  import treeInfo._

  /**
   *  Reify a type.
   *  For internal use only, use ``reified'' instead.
   */
  def reifyType(tpe0: Type): Tree = {
    assert(tpe0 != null, "tpe is null")
    val tpe = tpe0.dealias

    if (tpe.isErroneous)
      CannotReifyErroneousReifee(tpe)
    if (tpe.isLocalToReifee)
      CannotReifyType(tpe)

    // [Eugene] how do I check that the substitution is legal w.r.t tpe.info?
    val spliced = spliceType(tpe)
    if (spliced != EmptyTree)
      return spliced

    val tsym = tpe.typeSymbol
    if (tsym.isClass && tpe == tsym.typeConstructor && tsym.isStatic)
      Select(reify(tpe.typeSymbol), nme.asTypeConstructor)
    else tpe match {
      case tpe @ NoType =>
        reifyMirrorObject(tpe)
      case tpe @ NoPrefix =>
        reifyMirrorObject(tpe)
      case tpe @ ThisType(root) if root == RootClass =>
        mirrorSelect("definitions.RootClass.thisPrefix")
      case tpe @ ThisType(empty) if empty == EmptyPackageClass =>
        mirrorSelect("definitions.EmptyPackageClass.thisPrefix")
      case tpe @ ThisType(clazz) if clazz.isModuleClass && clazz.isStatic =>
        mirrorCall(nme.thisModuleType, reify(clazz.fullName))
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

  /** An obscure flag necessary for implicit TypeTag generation */
  private var spliceTypesEnabled = !dontSpliceAtTopLevel

  /** Keeps track of whether this reification contains abstract type parameters */
  private var _reificationIsConcrete = true
  def reificationIsConcrete = _reificationIsConcrete
  def reificationIsConcrete_=(value: Boolean) {
    _reificationIsConcrete = value
    if (!value && concrete) {
      assert(current.isInstanceOf[Type], current)
      val offender = current.asInstanceOf[Type]
      CannotReifyConcreteTypeTagHavingUnresolvedTypeParameters(offender)
    }
  }

  private type SpliceCacheKey = (Symbol, Symbol)
  private lazy val spliceCache: collection.mutable.Map[SpliceCacheKey, Tree] = {
    val cache = analyzer.perRunMacroCache.getOrElseUpdate(MacroContextReify, collection.mutable.Map[Any, Any]())
    cache.getOrElseUpdate("spliceCache", collection.mutable.Map[SpliceCacheKey, Tree]()).asInstanceOf[collection.mutable.Map[SpliceCacheKey, Tree]]
  }

  def spliceType(tpe: Type): Tree = {
    // [Eugene] it seems that depending on the context the very same symbol can be either a spliceable tparam or a quantified existential. very weird!
    val quantified = currentQuantified
    if (tpe.isSpliceable && !(quantified contains tpe.typeSymbol)) {
      if (reifyDebug) println("splicing " + tpe)

      if (spliceTypesEnabled) {
        var tagClass = if (concrete) ConcreteTypeTagClass else TypeTagClass
        val tagTpe = singleType(prefix.tpe, prefix.tpe member tagClass.name)

        // [Eugene] this should be enough for an abstract type, right?
        val key = (tagClass, tpe.typeSymbol)
        if (reifyDebug && spliceCache.contains(key)) println("cache hit: " + spliceCache(key))
        val result = spliceCache.getOrElseUpdate(key, {
          // if this fails, it might produce the dreaded "erroneous or inaccessible type" error
          // to find out the whereabouts of the error run scalac with -Ydebug
          if (reifyDebug) println("launching implicit search for %s.%s[%s]".format(prefix, tagClass.name, tpe))
          typer.resolveTypeTag(prefix.tpe, tpe, defaultErrorPosition, concrete) match {
            case failure if failure.isEmpty =>
              if (reifyDebug) println("implicit search was fruitless")
              EmptyTree
            case success =>
              if (reifyDebug) println("implicit search has produced a result: " + success)
              reificationIsConcrete &= concrete
              var splice = Select(success, nme.tpe)
              splice match {
                case InlinedTypeSplice(_, inlinedSymbolTable, tpe) =>
                  // all free vars local to the enclosing reifee should've already been inlined by ``Metalevels''
                  inlinedSymbolTable collect { case freedef @ FreeDef(_, _, binding, _, _) if binding.symbol.isLocalToReifee => assert(false, freedef) }
                  symbolTable ++= inlinedSymbolTable
                  reifyTrace("inlined the splicee: ")(tpe)
                case tpe =>
                  tpe
              }
          }
        })
        if (result != EmptyTree) return result.duplicate
      } else {
        if (reifyDebug) println("splicing has been cancelled: spliceTypesEnabled = false")
      }

      reificationIsConcrete = false
    }

    spliceTypesEnabled = true
    EmptyTree
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