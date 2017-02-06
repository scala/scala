package xsbt

import scala.tools.nsc.Global

trait GlobalHelpers {
  val global: Global
  import global._

  /** Return true if type shall be ignored, false otherwise. */
  @inline def ignoredType(tpe: Type) = {
    tpe == null ||
      tpe == NoType ||
      tpe.typeSymbol == EmptyPackageClass
  }

  /** Return true if symbol shall be ignored, false otherwise. */
  @inline def ignoredSymbol(symbol: Symbol) = {
    symbol == null ||
      symbol == NoSymbol ||
      symbol == EmptyPackageClass
  }

  /** Return true if name is empty, false otherwise. */
  def isEmptyName(name: Name): Boolean = {
    name match {
      case nme.EMPTY | nme.EMPTY_PACKAGE_NAME |
        tpnme.EMPTY | tpnme.EMPTY_PACKAGE_NAME => true
      case _ => false
    }
  }

  /** Apply `op` on every type symbol which doesn't represent a package. */
  def foreachNotPackageSymbolInType(tpe: Type)(op: Symbol => Unit): Unit = {
    new ForEachTypeTraverser(_ match {
      case null =>
      case tpe =>
        val sym = tpe.typeSymbolDirect
        if (!sym.hasPackageFlag) op(sym)
    }).traverse(tpe)
  }

  /** Returns true if given tree contains macro attchment. In such case calls func on tree from attachment. */
  def processMacroExpansion(in: Tree)(func: Tree => Unit): Boolean = {
    // Hotspot
    var seen = false
    in.attachments.all.foreach {
      case _ if seen =>
      case macroAttachment: analyzer.MacroExpansionAttachment =>
        func(macroAttachment.expandee)
        seen = true
      case _ =>
    }
    seen
  }

  object MacroExpansionOf {
    def unapply(tree: Tree): Option[Tree] = {
      tree.attachments.all.collect {
        case att: analyzer.MacroExpansionAttachment => att.expandee
      }.headOption
    }
  }
}
