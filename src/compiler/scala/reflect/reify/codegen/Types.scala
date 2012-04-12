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
//        reifyAnnotatedType(tpe)
        CannotReifyType(tpe)
      case _ =>
//        reifyToughType(tpe)
        CannotReifyType(tpe)
    }
  }

  /** An obscure flag necessary for implicit TypeTag generation */
  private var spliceTypesEnabled = !dontSpliceAtTopLevel

  /** Keeps track of whether this reification contains abstract type parameters */
  var maybeConcrete = true
  var definitelyConcrete = true

  def eligibleForSplicing(tpe: Type): Boolean = {
    // [Eugene] is this comprehensive?
    // the only thingies that we want to splice are: 1) type parameters, 2) type members
    // this check seems to cover them all, right?
    tpe.isInstanceOf[TypeRef] && tpe.typeSymbol.isAbstractType
  }

  private type SpliceCacheKey = (Symbol, Symbol)
  private lazy val spliceCache: collection.mutable.Map[SpliceCacheKey, Tree] = {
    val cache = analyzer.perRunMacroCache.getOrElseUpdate(MacroContextReify, collection.mutable.Map[Any, Any]())
    cache.getOrElseUpdate("spliceCache", collection.mutable.Map[SpliceCacheKey, Tree]()).asInstanceOf[collection.mutable.Map[SpliceCacheKey, Tree]]
  }

  def spliceType(tpe: Type): Tree = {
    if (eligibleForSplicing(tpe)) {
      if (reifyDebug) println("splicing " + tpe)

      if (spliceTypesEnabled) {
        var tagClass = if (requireConcreteTypeTag) ConcreteTypeTagClass else TypeTagClass
        val tagTpe = singleType(prefix.tpe, prefix.tpe member tagClass.name)

        // [Eugene] this should be enough for an abstract type, right?
        val key = (tagClass, tpe.typeSymbol)
        if (reifyDebug && spliceCache.contains(key)) println("cache hit: " + spliceCache(key))
        val result = spliceCache.getOrElseUpdate(key, {
          // if this fails, it might produce the dreaded "erroneous or inaccessible type" error
          // to find out the whereabouts of the error run scalac with -Ydebug
          if (reifyDebug) println("launching implicit search for %s.%s[%s]".format(prefix, tagClass.name, tpe))
          val positionBearer = mirror.analyzer.openMacros.find(c => c.macroApplication.pos != NoPosition).map(_.macroApplication).getOrElse(EmptyTree).asInstanceOf[Tree]
          typer.resolveTypeTag(positionBearer, prefix.tpe, tpe, requireConcreteTypeTag) match {
            case failure if failure.isEmpty =>
              if (reifyDebug) println("implicit search was fruitless")
              definitelyConcrete &= false
              maybeConcrete &= false
              EmptyTree
            case success =>
              if (reifyDebug) println("implicit search has produced a result: " + success)
              definitelyConcrete |= requireConcreteTypeTag
              maybeConcrete |= true
              var splice = Select(success, nme.tpe)
              splice match {
                case InlinedTypeSplice(_, inlinedSymbolTable, tpe) =>
                  // all free vars local to the enclosing reifee should've already been inlined by ``Metalevels''
                  inlinedSymbolTable foreach { case freedef @ FreeDef(_, _, binding, _) => assert(!binding.symbol.isLocalToReifee, freedef) }
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

      if (requireConcreteTypeTag)
        CannotReifyConcreteTypeTagHavingUnresolvedTypeParameters(tpe)
    }

    spliceTypesEnabled = true
    EmptyTree
  }

  // yet another thingie disabled for simplicity
  // in principle, we could retain and reify AnnotatedTypes
  // but that'd require reifying every type and symbol inside ann.args
  // however, since we've given up on tough types for the moment, the former would be problematic
//  private def reifyAnnotatedType(tpe: AnnotatedType): Tree = {
//    // ``Reshaper'' transforms annotation infos from symbols back into Modifier.annotations, which are trees
//    // so the only place on Earth that can lead to reification of AnnotationInfos is the Ay Tee Land
//    // therefore this function is as local as possible, don't move it out of this scope
//    def reifyAnnotationInfo(ann: AnnotationInfo): Tree = {
//      val reifiedArgs = ann.args map { arg =>
//        val saved1 = reifyTreeSymbols
//        val saved2 = reifyTreeTypes
//
//        try {
//          // one more quirk of reifying annotations
//          //
//          // when reifying AnnotatedTypes we need to reify all the types and symbols of inner ASTs
//          // that's because a lot of logic expects post-typer trees to have non-null tpes
//          //
//          // Q: reified trees are pre-typer, so there's shouldn't be a problem.
//          //    reflective typechecker will fill in missing symbols and types, right?
//          // A: actually, no. annotation ASTs live inside AnnotatedTypes,
//          //    and insides of the types is the place where typechecker doesn't look.
//          reifyTreeSymbols = true
//          reifyTreeTypes = true
//
//          // todo. every AnnotationInfo is an island, entire of itself
//          // no regular Traverser or Transformer can reach it
//          // hence we need to run its contents through the entire reification pipeline
//          // e.g. to apply reshaping or to check metalevels
//          reify(arg)
//        } finally {
//          reifyTreeSymbols = saved1
//          reifyTreeTypes = saved2
//        }
//      }
//
//      def reifyClassfileAnnotArg(arg: ClassfileAnnotArg): Tree = arg match {
//        case LiteralAnnotArg(const) =>
//          mirrorFactoryCall(nme.LiteralAnnotArg, reifyProduct(const))
//        case ArrayAnnotArg(args) =>
//          mirrorFactoryCall(nme.ArrayAnnotArg, scalaFactoryCall(nme.Array, args map reifyClassfileAnnotArg: _*))
//        case NestedAnnotArg(ann) =>
//          mirrorFactoryCall(nme.NestedAnnotArg, reifyAnnotationInfo(ann))
//      }
//
//      // if you reify originals of anns, you get SO when trying to reify AnnotatedTypes, so screw it - after all, it's not that important
//      val reifiedAssocs = ann.assocs map (assoc => scalaFactoryCall(nme.Tuple2, reify(assoc._1), reifyClassfileAnnotArg(assoc._2)))
//      mirrorFactoryCall(nme.AnnotationInfo, reify(ann.atp), mkList(reifiedArgs), mkList(reifiedAssocs))
//    }
//
//    val AnnotatedType(anns, underlying, selfsym) = tpe
//    mirrorFactoryCall(nme.AnnotatedType, mkList(anns map reifyAnnotationInfo), reify(underlying), reify(selfsym))
//  }

  // previous solution to reifying tough types involved creating dummy symbols (see ``registerReifiableSymbol'' calls below)
  // however such symbols lost all the connections with their origins and became almost useless, except for typechecking
  // hence this approach was replaced by less powerful, but more principled one based on ``reifyFreeType''
  // it's possible that later on we will revise and revive ``reifyToughType'', but for now it's disabled under an implementation restriction
//  /** Reify a tough type, i.e. the one that leads to creation of auxiliary symbols */
//  // This is the uncharted territory in the reifier
//  private def reifyToughType(tpe: Type): Tree = {
//    if (reifyDebug) println("tough type: %s (%s)".format(tpe, tpe.kind))
//
//    def reifyScope(scope: Scope): Tree = {
//      scope foreach registerReifiableSymbol
//      mirrorCall(nme.newScopeWith, scope.toList map reify: _*)
//    }
//
//    tpe match {
//      case tpe @ RefinedType(parents, decls) =>
//        registerReifiableSymbol(tpe.typeSymbol)
//        mirrorFactoryCall(tpe, reify(parents), reifyScope(decls), reify(tpe.typeSymbol))
//      case tpe @ ExistentialType(tparams, underlying) =>
//        tparams foreach registerReifiableSymbol
//        mirrorFactoryCall(tpe, reify(tparams), reify(underlying))
//      case tpe @ ClassInfoType(parents, decls, clazz) =>
//        registerReifiableSymbol(clazz)
//        mirrorFactoryCall(tpe, reify(parents), reifyScope(decls), reify(tpe.typeSymbol))
//      case tpe @ MethodType(params, restpe) =>
//        params foreach registerReifiableSymbol
//        mirrorFactoryCall(tpe, reify(params), reify(restpe))
//      case tpe @ PolyType(tparams, underlying) =>
//        tparams foreach registerReifiableSymbol
//        mirrorFactoryCall(tpe, reify(tparams), reify(underlying))
//      case _ =>
//        throw new Error("internal error: %s (%s) is not supported".format(tpe, tpe.kind))
//    }
//  }
}