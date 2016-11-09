package xsbt

import scala.tools.nsc.Global

trait GlobalHelpers {
  val global: Global
  import global._

  def symbolsInType(tp: Type): Set[Symbol] = {
    val typeSymbolCollector =
      new CollectTypeCollector({
        case tpe if (tpe != null) && !tpe.typeSymbolDirect.hasPackageFlag => tpe.typeSymbolDirect
      })

    typeSymbolCollector.collect(tp).toSet
  }

  def foreachSymbolInType(tpe: Type)(op: Symbol => Unit): Unit = {
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
