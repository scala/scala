package xsbt

import scala.tools.nsc.Global

trait GlobalHelpers {
  val global: Global
  import global.{ analyzer, Tree }

  object MacroExpansionOf {
    def unapply(tree: Tree): Option[Tree] = {
      tree.attachments.all.collect {
        case att: analyzer.MacroExpansionAttachment => att.expandee
      }.headOption
    }
  }
}
