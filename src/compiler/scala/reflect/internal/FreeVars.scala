package scala.reflect
package internal

trait FreeVars extends api.FreeVars {
  self: SymbolTable =>

  object FreeTerm extends FreeTermExtractor {
    def unapply(freeTerm: FreeTerm): Option[(TermName, Type, Any, String)] =
      Some(freeTerm.name, freeTerm.info, freeTerm.value, freeTerm.origin)
  }

  object FreeType extends FreeTypeExtractor {
    def unapply(freeType: FreeType): Option[(TypeName, Type, String)] =
      Some(freeType.name, freeType.info, freeType.origin)
  }

  // [Eugene] am I doing this right?
  def freeTerms(tree: Tree): List[FreeTerm] = {
    def isFreeTermSym(sym: Symbol) = sym != null && sym.isFreeTerm
    def isFreeTermTpe(t: Type) = t != null && isFreeTermSym(t.termSymbol)

    val buf = collection.mutable.Set[Symbol]()
    tree foreach (sub => {
      if (sub.tpe != null) buf ++= (sub.tpe collect { case tpe if isFreeTermTpe(tpe) => tpe.typeSymbol })
      if (sub.symbol != null && isFreeTermSym(sub.symbol)) buf += sub.symbol
    })

    buf.toList.collect{ case fty: FreeTerm => fty }
  }

  // [Eugene] am I doing this right?
  def freeTypes(tree: Tree): List[FreeType] = {
    def isFreeTypeSym(sym: Symbol) = sym != null && sym.isFreeType
    def isFreeTypeTpe(t: Type) = t != null && isFreeTypeSym(t.typeSymbol)

    val buf = collection.mutable.Set[Symbol]()
    tree foreach (sub => {
      if (sub.tpe != null) buf ++= (sub.tpe collect { case tpe if isFreeTypeTpe(tpe) => tpe.typeSymbol })
      if (sub.symbol != null && isFreeTypeSym(sub.symbol)) buf += sub.symbol
    })

    buf.toList.collect{ case fty: FreeType => fty }
  }

  // todo. also update tpe's of dependent free vars
  // e.g. if we substitute free$C, then free$C$this should have its info updated
  // todo. should also transform typetags of types dependent on that free type?
  // [Eugene] how do I check that the substitution is legal w.r.t fty.info?
  def substituteFreeTypes(tree0: Tree, subs: Map[FreeType, Type]): Tree = {
    val tree = tree0.duplicate
    new TreeTypeSubstituter(subs.keys.toList, subs.values.toList).traverse(tree)
    tree
  }

  // [Eugene] how do I check that the substitution is legal w.r.t fty.info?
  def substituteFreeTypes(tpe0: Type, subs: Map[FreeType, Type]): Type = {
    val tpe = tpe0 // [Eugene] tpe0.duplicate?
    tpe.subst(subs.keys.toList, subs.values.toList)
  }
}