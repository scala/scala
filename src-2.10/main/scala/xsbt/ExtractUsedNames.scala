package xsbt

import scala.tools.nsc._

/**
 * Extracts simple names used in given compilation unit.
 *
 * Extracts simple (unqualified) names mentioned in given in non-definition position by collecting
 * all symbols associated with non-definition trees and extracting names from all collected symbols.
 *
 * If given symbol is mentioned both in definition and in non-definition position (e.g. in member
 * selection) then that symbol is collected. It means that names of symbols defined and used in the
 * same compilation unit are extracted. We've considered not extracting names of those symbols
 * as an optimization strategy. It turned out that this is not correct.  Check
 * https://github.com/gkossakowski/sbt/issues/3 for an example of scenario where it matters.
 *
 * All extracted names are returned in _decoded_ form. This way we stay consistent with the rest
 * of incremental compiler which works with names in decoded form.
 *
 * Names mentioned in Import nodes are handled properly but require some special logic for two
 * reasons:
 *
 *   1. import node itself has a term symbol associated with it with a name `<import`>.
 *      I (gkossakowski) tried to track down what role this symbol serves but I couldn't.
 *      It doesn't look like there are many places in Scala compiler that refer to
 *      that kind of symbols explicitly.
 *   2. ImportSelector is not subtype of Tree therefore is not processed by `Tree.foreach`
 *
 * Another type of tree nodes that requires special handling is TypeTree. TypeTree nodes
 * has a little bit odd representation:
 *
 *   1. TypeTree.hasSymbol always returns false even when TypeTree.symbol
 *      returns a symbol
 *   2. The original tree from which given TypeTree was derived is stored
 *      in TypeTree.original but Tree.forech doesn't walk into original
 *      tree so we missed it
 *
 * The tree walking algorithm walks into TypeTree.original explicitly.
 *
 */
class ExtractUsedNames[GlobalType <: CallbackGlobal](val global: GlobalType) extends Compat {
  import global._

  def extract(unit: CompilationUnit): Set[String] = {
    val tree = unit.body
    val extractedByTreeWalk = extractByTreeWalk(tree)
    extractedByTreeWalk
  }

  private def extractByTreeWalk(tree: Tree): Set[String] = {
    val namesBuffer = collection.mutable.ListBuffer.empty[String]

    /*
     * Some macros appear to contain themselves as original tree.
     * We must check that we don't inspect the same tree over and over.
     * See https://issues.scala-lang.org/browse/SI-8486
     *     https://github.com/sbt/sbt/issues/1237
     *     https://github.com/sbt/sbt/issues/1544
     */
    val inspectedOriginalTrees = collection.mutable.Set.empty[Tree]

    def addSymbol(symbol: Symbol): Unit = {
      val symbolNameAsString = symbol.name.decode.trim
      namesBuffer += symbolNameAsString
      ()
    }

    def handleTreeNode(node: Tree): Unit = {
      def handleMacroExpansion(original: Tree): Unit = {
        original.foreach(handleTreeNode)
      }

      def handleClassicTreeNode(node: Tree): Unit = node match {
        case _: DefTree | _: Template => ()
        // turns out that Import node has a TermSymbol associated with it
        // I (Grzegorz) tried to understand why it's there and what does it represent but
        // that logic was introduced in 2005 without any justification I'll just ignore the
        // import node altogether and just process the selectors in the import node
        case Import(_, selectors: List[ImportSelector]) =>
          def usedNameInImportSelector(name: Name): Unit = {
            if ((name != null) && (name != nme.WILDCARD)) namesBuffer += name.toString
            ()
          }
          selectors foreach { selector =>
            usedNameInImportSelector(selector.name)
            usedNameInImportSelector(selector.rename)
          }
        // TODO: figure out whether we should process the original tree or walk the type
        // the argument for processing the original tree: we process what user wrote
        // the argument for processing the type: we catch all transformations that typer applies
        // to types but that might be a bad thing because it might expand aliases eagerly which
        // not what we need
        case t: TypeTree if t.original != null =>
          t.original.foreach(handleTreeNode)
        case t if t.hasSymbol && eligibleAsUsedName(t.symbol) =>
          addSymbol(t.symbol)
        case _ => ()
      }

      node match {
        case MacroExpansionOf(original) if inspectedOriginalTrees.add(original) =>
          handleClassicTreeNode(node)
          handleMacroExpansion(original)
        case _ =>
          handleClassicTreeNode(node)
      }
    }

    tree.foreach(handleTreeNode)
    namesBuffer.toSet
  }

  /**
   * Needed for compatibility with Scala 2.8 which doesn't define `tpnme`
   */
  private object tpnme {
    val EMPTY = nme.EMPTY.toTypeName
    val EMPTY_PACKAGE_NAME = nme.EMPTY_PACKAGE_NAME.toTypeName
  }

  private def eligibleAsUsedName(symbol: Symbol): Boolean = {
    def emptyName(name: Name): Boolean = name match {
      case nme.EMPTY | nme.EMPTY_PACKAGE_NAME | tpnme.EMPTY | tpnme.EMPTY_PACKAGE_NAME => true
      case _ => false
    }

    (symbol != NoSymbol) &&
      !symbol.isSynthetic &&
      !emptyName(symbol.name)
  }
}
