package scala.reflect.reify
package phases

trait Calculate {
  self: Reifier =>

  import global._

  implicit class RichCalculateSymbol(sym: Symbol) {
    def metalevel: Int = { assert(sym != null && sym != NoSymbol); localSymbols.getOrElse(sym, 0) }
    def isLocalToReifee = (localSymbols contains sym) // todo. how do I account for local skolems?
  }

  implicit class RichCalculateType(tpe: Type) {
    def isLocalToReifee = tpe != null && (tpe exists (tp => (localSymbols contains tp.typeSymbol) || (localSymbols contains tp.termSymbol)))
  }

  private def localSymbols: Map[Symbol, Int] = state.localSymbols // set of all symbols that are local to the tree to be reified
  private def localSymbols_=(value: Map[Symbol, Int]): Unit = state.localSymbols = value
  private def registerLocalSymbol(sym: Symbol, metalevel: Int): Unit =
    if (sym != null && sym != NoSymbol) {
      if (localSymbols contains sym)
        assert(localSymbols(sym) == metalevel, "metalevel mismatch: expected %s, actual %s".format(localSymbols(sym), metalevel))
      else
        localSymbols += (sym -> metalevel)
    }

  /**
   *  Merely traverses the reifiee and records symbols local to the reifee along with their metalevels.
   */
  val calculate = new Traverser {
    // see the explanation of metalevels in `Metalevels`
    var currMetalevel = 1

    override def traverse(tree: Tree): Unit = tree match {
      case TreeSplice(_) =>
        currMetalevel -= 1
        try super.traverse(tree)
        finally currMetalevel += 1
      case tree if tree.isDef =>
        if (reifyDebug) println("boundSym: %s of type %s".format(tree.symbol, (tree.productIterator.toList collect { case tt: TypeTree => tt }).headOption.getOrElse(TypeTree(tree.tpe))))
        registerLocalSymbol(tree.symbol, currMetalevel)

        bindRelatedSymbol(tree.symbol.sourceModule, "sourceModule")
        bindRelatedSymbol(tree.symbol.moduleClass, "moduleClass")
        bindRelatedSymbol(tree.symbol.companionClass, "companionClass")
        bindRelatedSymbol(tree.symbol.companionModule, "companionModule")
        Some(tree.symbol) collect { case termSymbol: TermSymbol => bindRelatedSymbol(termSymbol.referenced, "referenced") }
        Some(tree) collect { case labelDef: LabelDef => labelDef.params foreach (param => bindRelatedSymbol(param.symbol, "labelParam")) }
        def bindRelatedSymbol(related: Symbol, name: String): Unit =
          if (related != null && related != NoSymbol) {
            if (reifyDebug) println("boundSym (" + name + "): " + related)
            registerLocalSymbol(related, currMetalevel)
          }
        super.traverse(tree)
      case _ =>
        super.traverse(tree)
    }
  }
}
