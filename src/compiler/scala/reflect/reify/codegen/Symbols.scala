package scala.reflect.reify
package codegen

trait Symbols {
  self: Reifier =>

  import mirror._
  import definitions._
  import treeInfo._

  /** Reify a reference to a symbol */
  def reifySymRef(sym0: Symbol): Tree = {
    assert(sym0 != null, "sym is null")
    val sym = sym0.dealias

    if (sym == NoSymbol)
      mirrorSelect(nme.NoSymbol)
    else if (sym == RootPackage)
      Select(mirrorSelect(nme.definitions), nme.RootPackage)
    else if (sym == RootClass)
      Select(mirrorSelect(nme.definitions), nme.RootClass)
    else if (sym == EmptyPackage)
      Select(mirrorSelect(nme.definitions), nme.EmptyPackage)
    else if (sym == EmptyPackageClass)
      Select(mirrorSelect(nme.definitions), nme.EmptyPackageClass)
    else if (sym.isModuleClass)
      Select(reify(sym.sourceModule), nme.moduleClass)
    else if (sym.isLocatable) {
      // [Eugene] am I doing this right?
//      if (sym.isStaticOwner) { // no good for us, because it returns false for packages
      if (sym.isStatic && (sym.isClass || sym.isModule)) {
        val resolver = if (sym.isType) nme.staticClass else nme.staticModule
        mirrorCall(resolver, reify(sym.fullName))
      } else {
        if (reifyDebug) println("Locatable: %s (%s) owned by %s (%s) at %s".format(sym, sym.accurateKindString, sym.owner, sym.owner.accurateKindString, sym.owner.fullNameString))
        val rowner = reify(sym.owner)
        val rname = reify(sym.name.toString)
        if (sym.isType)
          mirrorCall(nme.selectType, rowner, rname)
        else if (sym.isMethod && sym.owner.isClass && sym.owner.info.decl(sym.name).isOverloaded) {
          val index = sym.owner.info.decl(sym.name).alternatives indexOf sym
          assert(index >= 0, sym)
          mirrorCall(nme.selectOverloadedMethod, rowner, rname, reify(index))
        } else
          mirrorCall(nme.selectTerm, rowner, rname)
      }
    } else {
      // todo. make sure that free methods and free local defs work correctly
      if (sym.isTerm) {
        if (reifyDebug) println("Free term" + (if (sym.isCapturedVariable) " (captured)" else "") + ": " + sym)
        reifyFreeTerm(sym, Ident(sym))
      } else {
        if (reifyDebug) println("Free type: " + sym)
        reifyFreeType(sym, Ident(sym))
      }
    }
  }

  def reifyFreeTerm(sym: Symbol, value: Tree): Tree =
    locallyReified get sym match {
      case Some(reified) =>
        reified
      case None =>
        if (sym.isCapturedVariable) {
          assert(value.isInstanceOf[Ident], showRaw(value))
          val capturedTpe = capturedVariableType(sym)
          val capturedValue = referenceCapturedVariable(sym)
          locallyReify(sym, mirrorCall(nme.newFreeTerm, reify(sym.name.toString), reify(capturedTpe), capturedValue, reify(origin(sym))))
        } else {
          locallyReify(sym, mirrorCall(nme.newFreeTerm, reify(sym.name.toString), reify(sym.tpe), value, reify(origin(sym))))
        }
    }

  def reifyFreeType(sym: Symbol, value: Tree): Tree =
    locallyReified get sym match {
      case Some(reified) =>
        reified
      case None =>
        val phantomTypeTag = Apply(TypeApply(Select(Ident(nme.MIRROR_SHORT), nme.TypeTag), List(value)), List(Literal(Constant(null))))
        // todo. implement info reification for free types: type bounds, HK-arity, whatever else that can be useful
        locallyReify(sym, mirrorCall(nme.newFreeType, reify(sym.name.toString), reify(sym.info), phantomTypeTag, reify(origin(sym))))
    }

  import scala.collection.mutable._
  private val localReifications = ArrayBuffer[ValDef]()
  private val locallyReified = Map[Symbol, Tree]()
  def symbolTable: List[ValDef] = localReifications.toList
  def symbolTable_=(newSymbolTable: List[ValDef]): Unit = {
    localReifications.clear()
    locallyReified.clear()
    newSymbolTable foreach {
      case freedef @ FreeDef(_, name, binding, _) =>
        if (!(locallyReified contains binding.symbol)) {
          localReifications += freedef
          locallyReified(binding.symbol) = Ident(name)
        }
    }
  }

  private def locallyReify(sym: Symbol, reificode: => Tree): Tree = {
    val reified = reificode
    val Apply(Select(_, flavor), _) = reified
    // [Eugene] name clashes are impossible, right?
    var name = newTermName(nme.MIRROR_FREE_PREFIX + sym.name)
    if (flavor == nme.newFreeTerm && sym.isType) name = name.append(nme.MIRROR_FREE_THIS_SUFFIX);
    // todo. also reify annotations for free vars
    localReifications += ValDef(NoMods, name, TypeTree(), reified)
    locallyReified(sym) = Ident(name)
    locallyReified(sym)
  }
}