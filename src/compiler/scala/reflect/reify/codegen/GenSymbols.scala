package scala.reflect.reify
package codegen

trait GenSymbols {
  self: Reifier =>

  import global._
  import definitions._

  /** Symbol table of the reifee.
   *
   *  Keeps track of auxiliary symbols that are necessary for this reification session.
   *  These include:
   *    1) Free vars (terms, types and existentials),
   *    2) Non-locatable symbols (sometimes, e.g. for RefinedTypes, we need to reify these; to do that we create their local copies in the reificode)
   *    3) Non-locatable symbols that are referred by #1, #2 and #3
   *
   *  Exposes three main methods:
   *    1) `syms` that lists symbols belonging to the table,
   *    2) `symXXX` family of methods that provide information about the symbols in the table,
   *    3) `encode` that renders the table into a list of trees (recursively populating #3 and setting up initialization code for #1, #2 and #3)
   */
  def symtab: SymbolTable = state.symtab

  /** Reify a reference to a symbol */
  def reifySymRef(sym: Symbol): Tree = {
    assert(sym != null, "sym is null")
    if (sym == NoSymbol)
      mirrorSelect(nme.NoSymbol)
    else if (sym.isRootPackage)
      mirrorMirrorSelect(nme.RootPackage)
    else if (sym.isRoot)
      mirrorMirrorSelect(nme.RootClass)
    else if (sym.isEmptyPackage)
      mirrorMirrorSelect(nme.EmptyPackage)
    else if (sym.isEmptyPackageClass)
      mirrorMirrorSelect(nme.EmptyPackageClass)
    else if (sym.isModuleClass)
      Select(Select(reify(sym.sourceModule), nme.asModuleSymbol), nme.moduleClass)
    else if (sym.isPackage)
      mirrorMirrorCall(nme.staticPackage, reify(sym.fullName))
    else if (sym.isLocatable) {
      /** This is a fancy conundrum that stems from the fact that Scala allows
       *  packageless packages and packageless objects with the same names in the same program.
       *
       *  For more details read the docs to staticModule and staticPackage.
       *  Here I'll just provide the examples of how reify works for different kinds of symbols.
       *
       *    // 1) packageless
       *    // packageless classes are non-ambiguous, but modules vs packages might be
       *    // that's why we have separate methods to reify those
       *    // note that staticModule will never resolve to a package if an object is missing and an homonymous package is present and vice versa
       *    // otherwise reification would be unsound
       *    class C => staticClass("C")
       *    object B => staticModule("B")
       *    package B => staticPackage("B")
       *
       *    // 2) classes and modules enclosed in a package
       *    // staticXXX methods always look into parent packages and ignores parent modules, so for fully qualified names they are non-ambiguous
       *    // namely even if there's an object B { class C } next to package B { class C }, then staticClass("B.C") will resolve to a packageful class
       *    // this closely mirrors Scala's behavior, read up the docs to staticModule/staticPackage for more information
       *    package B { class C } => staticClass("B.C")
       *    package B { object B } => staticModule("B.B")
       *    package B { package B } => staticPackage("B.B")
       *
       *    // 3) classes and modules enclosed in a packageless module
       *    // staticClass/staticModule won't look into EmptyPackageClass, so we reify such symbols in a roundabout way
       *    object B { class C } => selectType(staticModule("B"), "C")
       *    object B { object B } => selectType(staticModule("B"), "B")
       *    object B { package B } => impossible
       */
      val hasPackagelessParent = sym.ownerChain.tail.tail exists (_.isEmptyPackageClass)
      if (sym.isStatic && (sym.isClass || sym.isModule) && !hasPackagelessParent) {
        val resolver = if (sym.isType) nme.staticClass else nme.staticModule
        mirrorMirrorCall(resolver, reify(sym.fullName))
      } else {
        if (reifyDebug) println("Locatable: %s (%s) owned by %s (%s) at %s".format(sym, sym.accurateKindString, sym.owner, sym.owner.accurateKindString, sym.owner.fullNameString))
        val rowner = reify(sym.owner)
        val rname = reify(sym.name.toString)
        if (sym.isType)
          mirrorBuildCall(nme.selectType, rowner, rname)
        else if (sym.isMethod && sym.owner.isClass && sym.owner.info.decl(sym.name).isOverloaded) {
          val index = sym.owner.info.decl(sym.name).alternatives indexOf sym
          assert(index >= 0, sym)
          mirrorBuildCall(nme.selectOverloadedMethod, rowner, rname, reify(index))
        } else
          mirrorBuildCall(nme.selectTerm, rowner, rname)
      }
    } else {
      // todo. make sure that free methods and free local defs work correctly
      if (sym.isTerm) reifyFreeTerm(sym, Ident(sym))
      else reifyFreeType(sym, Ident(sym))
    }
  }

  def reifyFreeTerm(sym: Symbol, value: Tree): Tree =
    reifyIntoSymtab(sym) {
      if (reifyDebug) println("Free term" + (if (sym.isCapturedVariable) " (captured)" else "") + ": " + sym + "(" + sym.accurateKindString + ")")
      var name = newTermName(nme.REIFY_FREE_PREFIX + sym.name)
      if (sym.isType) name = name.append(nme.REIFY_FREE_THIS_SUFFIX)
      if (sym.isCapturedVariable) {
        assert(value.isInstanceOf[Ident], showRaw(value))
        val capturedTpe = capturedVariableType(sym)
        val capturedValue = referenceCapturedVariable(sym)
        (name, mirrorBuildCall(nme.newFreeTerm, reify(sym.name.toString), reify(capturedTpe), capturedValue, mirrorBuildCall(nme.flagsFromBits, reify(sym.flags)), reify(origin(sym))))
      } else {
        (name, mirrorBuildCall(nme.newFreeTerm, reify(sym.name.toString), reify(sym.tpe), value, mirrorBuildCall(nme.flagsFromBits, reify(sym.flags)), reify(origin(sym))))
      }
    }

  def reifyFreeType(sym: Symbol, value: Tree): Tree =
    reifyIntoSymtab(sym) {
      if (reifyDebug) println("Free type: %s (%s)".format(sym, sym.accurateKindString))
      var name = newTermName(nme.REIFY_FREE_PREFIX + sym.name)
      val phantomTypeTag = Apply(TypeApply(Select(Ident(nme.UNIVERSE_SHORT), nme.TypeTag), List(value)), List(Literal(Constant(null)), Literal(Constant(null))))
      val flavor = if (sym.isExistential) nme.newFreeExistential else nme.newFreeType
      (name, mirrorBuildCall(flavor, reify(sym.name.toString), reify(sym.info), phantomTypeTag, mirrorBuildCall(nme.flagsFromBits, reify(sym.flags)), reify(origin(sym))))
    }

  def reifySymDef(sym: Symbol): Tree =
    reifyIntoSymtab(sym) {
      if (reifyDebug) println("Sym def: %s (%s)".format(sym, sym.accurateKindString))
      assert(!sym.isLocatable, sym) // if this assertion fires, then tough type reification needs to be rethought
      sym.owner.ownersIterator find (!_.isLocatable) foreach reifySymDef
      var name = newTermName(nme.REIFY_SYMDEF_PREFIX + sym.name)
      (name, mirrorBuildCall(nme.newNestedSymbol, reify(sym.owner), reify(sym.name), reify(sym.pos), mirrorBuildCall(nme.flagsFromBits, reify(sym.flags)), reify(sym.isClass)))
    }

  private def reifyIntoSymtab(sym: Symbol)(reificode: => (TermName, Tree)): Tree ={
    def fromSymtab = symtab symRef sym
    if (fromSymtab == EmptyTree) {
      val reification = reificode
      state.symtab += (sym, reification._1, reification._2)
    }
    fromSymtab
  }
}
