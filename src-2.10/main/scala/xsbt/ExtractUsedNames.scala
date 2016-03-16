package xsbt

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
class ExtractUsedNames[GlobalType <: CallbackGlobal](val global: GlobalType) extends Compat with ClassName {
  import global._

  def extract(unit: CompilationUnit): Map[String, Set[String]] = {
    val tree = unit.body
    val traverser = new ExtractUsedNamesTraverser
    traverser.traverse(tree)
    val namesUsedAtTopLevel = traverser.namesUsedAtTopLevel
    if (namesUsedAtTopLevel.nonEmpty) {
      val classOrModuleDef = firstClassOrModuleDef(tree)
      classOrModuleDef match {
        case Some(classOrModuleDef) =>
          val sym = classOrModuleDef.symbol
          val firstClassSymbol = if (sym.isModule) sym.moduleClass else sym
          val firstClassName = className(firstClassSymbol)
          traverser.namesUsedInClasses(firstClassName) ++= namesUsedAtTopLevel
        case None =>
          unit.warning(
            NoPosition,
            """|Found names used at the top level but no class, trait or object is defined in the compilation unit.
               |The incremental compiler cannot record used names in such case.
               |Some errors like unused import referring to a non-existent class might not be reported.""".stripMargin
          )
      }
    }

    traverser.namesUsedInClasses.toMap
  }

  private def firstClassOrModuleDef(tree: Tree): Option[Tree] = {
    tree foreach {
      case t @ ((_: ClassDef) | (_: ModuleDef)) => return Some(t)
      case _                                    => ()
    }
    None
  }

  private class ExtractUsedNamesTraverser extends Traverser {
    val namesUsedInClasses = collection.mutable.Map.empty[String, Set[String]].withDefaultValue(Set.empty)
    val namesUsedAtTopLevel = collection.mutable.Set.empty[String]

    /*
     * Some macros appear to contain themselves as original tree.
     * We must check that we don't inspect the same tree over and over.
     * See https://issues.scala-lang.org/browse/SI-8486
     *     https://github.com/sbt/sbt/issues/1237
     *     https://github.com/sbt/sbt/issues/1544
     */
    private val inspectedOriginalTrees = collection.mutable.Set.empty[Tree]

    override def traverse(tree: Tree): Unit = tree match {
      case MacroExpansionOf(original) if inspectedOriginalTrees.add(original) =>
        handleClassicTreeNode(tree)
        handleMacroExpansion(original)
        super.traverse(tree)
      case _ =>
        handleClassicTreeNode(tree)
        super.traverse(tree)
    }

    private def addSymbol(symbol: Symbol): Unit = {
      addName(symbol.name)
    }

    private def addName(name: Name, enclosingNonLocalClass: Symbol = resolveEnclosingNonLocalClass): Unit = {
      val nameAsString = name.decode.trim
      if (enclosingNonLocalClass == NoSymbol || enclosingNonLocalClass.isPackage) {
        namesUsedAtTopLevel += nameAsString
      } else {
        val className = ExtractUsedNames.this.className(enclosingNonLocalClass)
        namesUsedInClasses(className) += nameAsString
      }
    }

    private def handleMacroExpansion(original: Tree): Unit = {
      original.foreach(traverse)
    }

    private def handleClassicTreeNode(tree: Tree): Unit = tree match {
      case _: DefTree | _: Template => ()
      // turns out that Import node has a TermSymbol associated with it
      // I (Grzegorz) tried to understand why it's there and what does it represent but
      // that logic was introduced in 2005 without any justification I'll just ignore the
      // import node altogether and just process the selectors in the import node
      case Import(_, selectors: List[ImportSelector]) =>
        val enclosingNonLocalClass = resolveEnclosingNonLocalClass
        def usedNameInImportSelector(name: Name): Unit =
          if ((name != null) && (name != nme.WILDCARD)) addName(name, enclosingNonLocalClass)
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
        t.original.foreach(traverse)
      case t if t.hasSymbol && eligibleAsUsedName(t.symbol) =>
        addSymbol(t.symbol)
      case _ =>
    }

    /**
     * Resolves a class to which we attribute a used name by getting the enclosing class
     * for `currentOwner` and then looking up the most inner enclosing class that is non local.
     * The second returned value indicates if the enclosing class for `currentOwner`
     * is a local class.
     */
    private def resolveEnclosingNonLocalClass: Symbol = {
      val fromClass = enclOrModuleClass(currentOwner)
      if (fromClass == NoSymbol || fromClass.isPackage)
        fromClass
      else {
        val fromNonLocalClass = localToNonLocalClass.resolveNonLocal(fromClass)
        assert(!(fromClass == NoSymbol || fromClass.isPackage))
        fromNonLocalClass
      }
    }

    private def enclOrModuleClass(s: Symbol): Symbol =
      if (s.isModule) s.moduleClass else s.enclClass
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
